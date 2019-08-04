#![allow(non_snake_case)]
//FIXME
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

use std::collections::HashMap;
use std::convert::From;
use std::iter::FromIterator;
use std::rc::Rc;
use std::str::FromStr;

use super::ast;
use super::ast::{Expr, ExprKind};
use super::schema;
use std::str;

use uuid::Uuid;

use nom::error::ErrorKind;
use nom::{Err, IResult, Needed};

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::character::*;
use nom::combinator::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;

use std::cell::Cell;

type Error = ();

enum QueryOption<'a> {
    Select(Vec<Rc<ast::QueryExpr<'a>>>),
    Expand(Vec<Rc<ast::QueryExpr<'a>>>),
    Filter(Rc<Expr<'a>>),
    Search(Rc<Expr<'a>>),
    Orderby(Vec<Rc<Expr<'a>>>),
    Skip(u32),
    Top(u32),
    Count(bool),
    Levels(u32),
    Compute(Vec<Rc<Expr<'a>>>),
    Params(Vec<Rc<Expr<'a>>>),
    Deltatoken(&'a str),
    Format(&'a str),
    Id(&'a str),
    Schemaversion(&'a str),
    Skiptoken(&'a str),
    Index(u32),
}

/// Expression result
fn expr<'a, F>(f: F) -> impl Fn(Input<'a>) -> ExprOutput<'a>
where
    F: Fn(Input<'a>) -> IResult<Input<'a>, ExprKind<'a>, Error>,
{
    move |input| match f(input.clone()) {
        Ok((input, kind)) => {
            let expr = Rc::new(input.parser.expr(kind));
            Ok((input, expr))
        }
        Err(e) => Err(e),
    }
}

use nom::{Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};
use std::ops::RangeTo;

#[derive(Clone, Debug)]
// If we don't derive Copy the compiler will scream at you if you try to reuse the same "slice"
// twice. Also, this struct should be as small as possible since we're cloning it all over the
// place
pub struct Input<'a> {
    parser: &'a Parser<'a>,
    data: &'a str,
}

impl nom::UnspecializedInput for Input<'_> {}

impl Input<'_> {
    fn to_string(&self) -> String {
        String::from(self.data)
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn split_at(&self, mid: usize) -> (Self, Self) {
        let (a, b) = self.data.split_at(mid);
        let a = Self {
            parser: self.parser,
            data: a,
        };
        let b = Self {
            parser: self.parser,
            data: b,
        };
        (a, b)
    }
}
impl InputTake for Input<'_> {
    fn take(&self, count: usize) -> Self {
        Self {
            parser: self.parser,
            data: InputTake::take(&self.data, count),
        }
    }
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (a, b) = InputTake::take_split(&self.data, count);
        (
            Self {
                parser: self.parser,
                data: a,
            },
            Self {
                parser: self.parser,
                data: b,
            },
        )
    }
}

impl<'a> InputIter for Input<'a> {
    type Item = <&'a str as InputIter>::Item;
    type Iter = <&'a str as InputIter>::Iter;
    type IterElem = <&'a str as InputIter>::IterElem;
    fn iter_indices(&self) -> Self::Iter {
        InputIter::iter_indices(&self.data)
    }
    fn iter_elements(&self) -> Self::IterElem {
        InputIter::iter_elements(&self.data)
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        InputIter::position(&self.data, predicate)
    }
    fn slice_index(&self, count: usize) -> Option<usize> {
        InputIter::slice_index(&self.data, count)
    }
}

impl Compare<&str> for Input<'_> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        Compare::compare(&self.data, t)
    }
    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        Compare::compare_no_case(&self.data, t)
    }
}

impl Slice<std::ops::RangeTo<usize>> for Input<'_> {
    fn slice(&self, range: std::ops::RangeTo<usize>) -> Self {
        Self {
            parser: self.parser,
            data: Slice::slice(&self.data, range),
        }
    }
}

impl Slice<std::ops::RangeFrom<usize>> for Input<'_> {
    fn slice(&self, range: std::ops::RangeFrom<usize>) -> Self {
        Self {
            parser: self.parser,
            data: Slice::slice(&self.data, range),
        }
    }
}

impl Offset for Input<'_> {
    fn offset(&self, second: &Self) -> usize {
        Offset::offset(&self.data, &second.data)
    }
}

impl InputLength for Input<'_> {
    fn input_len(&self) -> usize {
        InputLength::input_len(&self.data)
    }
}

impl std::cmp::PartialEq for Input<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl std::cmp::Eq for Input<'_> {}

type ExprOutput<'a> = IResult<Input<'a>, Rc<ast::Expr<'a>>, Error>;

//* ;------------------------------------------------------------------------------
//* ; OData ABNF Construction Rules Version 4.01
//* ;------------------------------------------------------------------------------
//* ; Working Draft 03
//* ; 28 September 2017
//* ;------------------------------------------------------------------------------
//* ;
//* ;     OData Version 4.01
//* ;     Committee Specification 01
//* ;     30 January 2018
//* ;     Copyright (c) OASIS Open 2018. All Rights Reserved.
//* ;     Source: http://docs.oasis-open.org/odata/odata/v4.01/cs01/abnf/
//* ;     Latest version of narrative specification: http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html
//* ;     TC IPR Statement: https://www.oasis-open.org/committees/odata/ipr.php
//* ;
//* ; Technical Committee:
//* ;   OASIS Open Data Protocol (OData) TC
//* ;   https://www.oasis-open.org/committees/odata
//* ;
//* ; Chairs:
//* ;   - Ralf Handl (ralf.handl@sap.com), SAP SE
//* ;   - Ram Jeyaraman (Ram.Jeyaraman@microsoft.com), Microsoft
//* ;
//* ; Editors:
//* ;   - Ralf Handl (ralf.handl@sap.com), SAP SE
//* ;   - Michael Pizzo (mikep@microsoft.com), Microsoft
//* ;   - Martin Zurmuehl (martin.zurmuehl@sap.com), SAP SE
//* ;
//* ; Additional artifacts:
//* ;   This grammar is one component of a Work Product which consists of:
//* ;   - OData Version 4.01 Part 1: Protocol
//* ;   - OData Version 4.01 Part 2: URL Conventions
//* ;   - OData ABNF Construction Rules Version 4.01 (this document)
//* ;   - OData ABNF Test Cases Version 4.01
//* ;
//* ; Related work:
//* ;   This specification replaces or supersedes:
//* ;   - OData ABNF Construction Rules Version 4.0
//* ;   This work product is related to
//* ;   - OData Common Schema Definition Language (CSDL) JSON Representation Version 4.01
//* ;   - OData Common Schema Definition Language (CSDL) XML Representation Version 4.01
//* ;   - OData JSON Format Version 4.01
//* ;
//* ; Abstract:
//* ;   The Open Data Protocol (OData) enables the creation of REST-based data
//* ;   services, which allow resources, identified using Uniform Resource
//* ;   Identifiers (URLs) and defined in a data model, to be published and
//* ;   edited by Web clients using simple HTTP messages. This document defines
//* ;   the URL syntax for requests and the serialization format for primitive
//* ;   literals in request and response payloads.
//* ;
//* ; Overview:
//* ;   This grammar uses the ABNF defined in RFC5234 with one extension: literals
//* ;   enclosed in single quotes (e.g. '$metadata') are treated case-sensitive.
//* ;
//* ;   The following rules assume that URIs have been percent-encoding normalized
//* ;   as described in section 6.2.2.2 of RFC3986
//* ;   (http://tools.ietf.org/html/rfc3986#section-6.2.2.2)
//* ;   before applying the grammar to them, i.e. all characters in the unreserved
//* ;   set (see rule "unreserved" below) are plain literals and NOT
//* ;   percent-encoded.
//* ;
//* ;   For characters outside the unreserved set the rules explicitly state
//* ;   whether the percent-encoded representation is treated identical to the
//* ;   plain literal representation.
//* ;
//* ;   One prominent example is the single quote that delimits OData primitive
//* ;   type literals: %27 and ' are treated identically, so a single quote within
//* ;   a string literal is "encoded" as two consecutive single quotes in either
//* ;   literal or percent-encoded representation.
//* ;
//* ; Contents:
//* ;   1. Resource Path
//* ;   2. Query Options
//* ;   3. Context URL Fragments
//* ;   4. Expressions
//* ;   5. JSON format for function parameters
//* ;   6. Names and identifiers
//* ;   7. Literal Data Values
//* ;   8. Header values
//* ;   9. Punctuation
//* ;
//* ;   A. URI syntax [RFC3986]
//* ;   B. IRI syntax [RFC3986]
//* ;   C. ABNF core definitions [RFC5234]
//* ;
//* ;------------------------------------------------------------------------------
//* dummyStartRule = odataUri / header / primitiveValue ; just to please the test parser
//* ;------------------------------------------------------------------------------
//*
//*
//* odataUri = serviceRoot [ odataRelativeUri ]

use std::cell::RefCell;

#[derive(Debug)]
struct Scope<'a> {
    /// Essentially a list of stack frames
    frames: RefCell<Vec<HashMap<&'a str, Rc<Expr<'a>>>>>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Scope {
            frames: RefCell::new(vec![HashMap::new()]),
        }
    }

    /// Walks the frames in reverse order until it finds a match
    fn resolve(&self, name: &'a str) -> Option<Rc<Expr<'a>>> {
        self.frames.borrow()
            .iter().rev()
            .find_map(|frame| frame.get(name).map(|e| e.to_owned()))
    }

    fn push(&self, name: &'a str, expr: Rc<Expr<'a>>) {
        let mut frames = self.frames.borrow_mut();
        frames.last_mut().unwrap().insert(name, expr);
    }

    fn push_frame(&self) {
        let mut frames = self.frames.borrow_mut();
        frames.push(HashMap::new());
    }

    fn pop_frame(&self) {
        let mut frames = self.frames.borrow_mut();
        frames.pop();
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    document: &'a schema::Document<'a>,
    next_node_id: Cell<ast::NodeId>,
    scope: Scope<'a>,
    unparsed_params: RefCell<HashMap<&'a str, &'a str>>,
}

impl<'a, 'b> Parser<'a> {
    pub fn new(document: &'a schema::Document<'a>) -> Self {
        Parser {
            document: document,
            next_node_id: Cell::new(1),
            scope: Scope::new(),
            unparsed_params: RefCell::new(HashMap::new()),
        }
    }

    pub fn expr(&self, node: ExprKind<'b>) -> ast::Expr<'b> {
        let expr = ast::Expr {
            id: self.next_node_id.get(),
            // FIXME is there a way to avoid the clone? I think the lifetimes could be relaxed a
            // bit to allow it
            ty: node.clone().to_ty(),
            node,
            // FIXME
        };

        self.next_node_id.set(expr.id + 1);
        expr
    }

    pub fn parse(&'a mut self, input: &'a str) -> IResult<Input<'a>, ast::ODataURI<'a>, Error> {
        let input = Input {
            parser: self,
            data: input,
        };

        odataUri(input, self.document)
    }
}

pub fn odataUri<'a>(
    input: Input<'a>,
    document: &'a schema::Document<'a>,
) -> IResult<Input<'a>, ast::ODataURI<'a>, Error> {
    // src: http://docs.oasis-open.org/odata/odata/v4.01/cs01/part2-url-conventions/odata-v4.01-cs01-part2-url-conventions.html#sec_URLComponents
    // Mandated and suggested content of these three significant URL components used by an OData
    // service are covered in sequence in the three following chapters.  OData follows the URI
    // syntax rules defined in [RFC3986] and in addition assigns special meaning to several of the
    // sub-delimiters defined by [RFC3986], so special care has to be taken regarding parsing and
    // percent-decoding.
    //
    // [RFC3986] defines three steps for URL processing that MUST be performed before percent-decoding:
    // ·       Split undecoded URL into components scheme, hier-part, query, and fragment at first ":", then first "?", and then first "#"
    // ·       Split undecoded hier-part into authority and path
    // ·       Split undecoded path into path segments at "/"
    //
    // After applying these steps defined by RFC3986 the following steps MUST be performed:
    // ·       Split undecoded query at "&" into query options, and each query option at the first "=" into query option name and query option value
    // ·       Percent-decode path segments, query option names, and query option values exactly once
    // ·       Interpret path segments, query option names, and query option values according to OData rules

    let (input, service_root) = serviceRoot(input, &document.service_root)?;
    let (input, relative_uri) = opt(|i| odataRelativeUri(i, &document))(input)?;

    Ok((
        input,
        ast::ODataURI {
            service_root: service_root.data,
            relative_uri,
        },
    ))
}
//*
//* serviceRoot = ( "https" / "http" )                    ; Note: case-insensitive
//*               "://" host [ ":" port ]
//*               "/" *( segment-nz "/" )
fn serviceRoot<'a>(input: Input<'a>, service_root: &'a str) -> IResult<Input<'a>, Input<'a>, Error> {
    // FIXME this should be relaxed to accept case-insensitive http(s), default ports, etc
    tag(service_root)(input)
}

//*
//* ; Note: dollar-prefixed path segments are case-sensitive!
//* odataRelativeUri = '$batch'  [ "?" batchOptions ]
//*                  / '$entity' "?" entityOptions
//*                  / '$entity' "/" qualifiedEntityTypeName "?" entityCastOptions
//*                  / '$metadata' [ "?" metadataOptions ] [ context ]
//*                  / resourcePath [ "?" queryOptions ]
fn odataRelativeUri<'a>(
    input: Input<'a>,
    doc: &'a schema::Document<'a>,
) -> IResult<Input<'a>, ast::RelativeURI<'a>, Error> {
    let (path, input) = input.split_at(input.data.find('?').unwrap_or(input.len()));
    let (query, fragment) = input.split_at(input.data.find('#').unwrap_or(input.len()));
    //FIXME we should split and decode path here isntead of parsing it as a string
    // let path = path.split('/');

    return alt((
        map(
            preceded(tag("$batch"), opt(preceded(tag("?"), batchOptions))),
            ast::RelativeURI::Batch,
        ),
        value(
            ast::RelativeURI::Entity,
            tuple((tag("$entity"), tag("?"), entityOptions)),
        ),
        value(
            ast::RelativeURI::Entity,
            tuple((
                tag("$entity/"),
                qualifiedEntityTypeName,
                tag("?"),
                entityCastOptions,
            )),
        ),
        value(
            ast::RelativeURI::Metadata,
            tuple((
                tag("$metadata"),
                opt(tuple((tag("?"), metadataOptions))),
                opt(context),
            )),
        ),
        |input: Input<'a>| {
            if query.data.len() > 0 {
                // separate scope to release the mut borrow as soon as we're done
                let mut unparsed_params = input.parser.unparsed_params.borrow_mut();
                for option in query.data[1..].split('&') {
                    if let Some(idx) = option.find('=') {
                        let (key, value) = option.split_at(idx);
                        let key = Input {
                            parser: input.parser,
                            data: key,
                        };
                        if let Ok((_, name)) = parameterAlias_wip2(key) {
                            unparsed_params.insert(name.data, &value[1..]);
                        }
                    }
                }
            }

            let (input, resource) = all_consuming(|i| resourcePath(i, &doc.entity_container))(path.clone())?;

            input.parser.scope.push("$it", resource.clone());
            input.parser.scope.push("$this", resource.clone());

            let mut resource_query = ast::ResourceQuery::new(resource.clone());

            {
                // separate scope to release the mut borrows as soon as we're done
                let mut unparsed_params = input.parser.unparsed_params.borrow_mut();

                for (name, value) in unparsed_params.drain() {
                    let value_input = Input{
                        parser: input.parser,
                        data: value,
                    };

                    // FIXME parameter values be arrayOrObj, not just commonExpr
                    let (_, value) = all_consuming(|i| commonExpr_wip(i, 0))(value_input)?;

                    input.parser.scope.push(name, value.clone());
                    resource_query.expr.params.push(value);
                }
            }

            // FIXME can we avoid the clone here?
            let (input, opts) = opt(preceded(tag("?"), |i| queryOptions_wip(i, resource_query.clone())))(query.clone())?;

            Ok((input, ast::RelativeURI::Resource(opts.unwrap_or(resource_query))))
        },
    ))(input);
}

//*
//*
//* ;------------------------------------------------------------------------------
//* ; 1. Resource Path
//* ;------------------------------------------------------------------------------
//*
//* resourcePath = entitySetName                  [ collectionNavigation ]
//*              / singletonEntity                [ singleNavigation ]
//*              / actionImportCall
//*              / entityColFunctionImportCall    [ collectionNavigation ]
//*              / entityFunctionImportCall       [ singleNavigation ]
//*              / complexColFunctionImportCall   [ complexColPath ]
//*              / complexFunctionImportCall      [ complexPath ]
//*              / primitiveColFunctionImportCall [ primitiveColPath ]
//*              / primitiveFunctionImportCall    [ primitivePath ]
//*              / functionImportCallNoParens
//*              / crossjoin
//*              / '$all'                         [ "/" qualifiedEntityTypeName ]
fn resourcePath<'a>(
    input: Input<'a>,
    entity_container: &'a schema::EntityContainer<'a>,
) -> ExprOutput<'a> {
    return alt((
		|i| {
			let (i, entity_set) = expr(map(|i| entitySetName_wip(i, entity_container), ExprKind::EntitySet))(i)?;
			let (i, nav) = opt(|i| collectionNavigation_wip(i, &entity_set))(i)?;

			Ok((i, nav.unwrap_or(entity_set)))
		},
		|i| {
			unimplemented!()
		}
		// value(vec![ast::PathSegment::Singleton], recognize(tuple((singletonEntity, opt(singleNavigation))))),
		// value(vec![ast::PathSegment::Action], actionImportCall),
		// value(vec![ast::PathSegment::Function], recognize(tuple((entityColFunctionImportCall, opt(collectionNavigation))))),
		// value(vec![ast::PathSegment::Function], recognize(tuple((entityFunctionImportCall, opt(singleNavigation))))),
		// value(vec![ast::PathSegment::Function], recognize(tuple((complexColFunctionImportCall, opt(complexColPath))))),
		// value(vec![ast::PathSegment::Function], recognize(tuple((complexFunctionImportCall, opt(complexPath))))),
		// value(vec![ast::PathSegment::Function], recognize(tuple((primitiveColFunctionImportCall, opt(primitiveColPath))))),
		// value(vec![ast::PathSegment::Function], recognize(tuple((primitiveFunctionImportCall, opt(primitivePath))))),
		// value(vec![ast::PathSegment::Function], functionImportCallNoParens),
		// value(vec![ast::PathSegment::Crossjoin], crossjoin),
		// value(vec![ast::PathSegment::All], recognize(tuple((tag("$all"), opt(tuple((tag("/"), qualifiedEntityTypeName))))))),
	))(input);
}

//*
//* collectionNavigation = [ "/" qualifiedEntityTypeName ] [ collectionNavPath ]
named!(collectionNavigation<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(collectionNavPath))))));
fn collectionNavigation_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, cast) = opt(preceded(tag("/"), |i| {
        qualifiedEntityTypeName_wip(i, child)
    }))(input)?;
    let (input, path) = opt(|i| collectionNavPath_wip(i, cast.as_ref().unwrap_or(child)))(input)?;

    // FIXME we should error out if none of the above matched. Also, one less Rc :)
    Ok((input, path.or(cast).unwrap_or_else(|| Rc::clone(child))))
}
//* collectionNavPath    = keyPredicate [ singleNavigation ]
//*                      / filterInPath [ collectionNavigation ]
//*                      / each [ boundOperation ]
//*                      / boundOperation
//*                      / count
//*                      / ref
//
//  errata: While in the ABNF the keyPredicate case appears first, we in fact have to do it last according
//  to the precence rules defined here:
//  http://docs.oasis-open.org/odata/odata/v4.01/cs01/part2-url-conventions/odata-v4.01-cs01-part2-url-conventions.html#sec_KeyasSegmentConvention
named!(collectionNavPath<Input, Input, Error>, call!(alt((recognize(tuple((keyPredicate, opt(singleNavigation))))
					   , recognize(tuple((filterInPath, opt(collectionNavigation))))
					   , recognize(tuple((each, opt(boundOperation))))
					   , boundOperation
					   , count
					   , _ref
					))));
fn collectionNavPath_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    alt((
        |i| {
            let (i, filter) = filterInPath_wip(i, child)?;
            let (i, path) = opt(|i| collectionNavigation_wip(i, &filter))(i)?;

            Ok((i, path.unwrap_or(filter)))
        },
        |i| {
            let (input, each) = each_wip2(i, child)?;
            let (input, bound_op) = opt(|i| boundOperation_wip2(i, &each))(input)?;

            Ok((input, bound_op.unwrap_or(each)))
        },
        |i| boundOperation_wip2(i, child),
        |i| count_wip2(i, child),
        |i| _ref_wip2(i, child),
        |i| {
            let (i, key) = keyPredicate_wip(i, child)?;
            // let (i, path) = opt(|i| singleNavigation_wip(i, &key))(i)?;

            Ok((i, key))
        },
    ))(input)
}

//*
//* keyPredicate     = simpleKey / compoundKey / keyPathSegments
named!(keyPredicate<Input, Input, Error>, call!(alt((simpleKey, compoundKey, keyPathSegments))));
fn keyPredicate_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    //FIXME typecheck we are actually operating on a keyed resource
    alt((
        |i| simpleKey_wip(i, child),
        |i| compoundKey_wip(i, child),
        |i| keyPathSegments_wip(i, child),
    ))(input)
}
//* simpleKey        = OPEN ( parameterAlias / keyPropertyValue ) CLOSE
named!(simpleKey<Input, Input, Error>, call!(recognize(tuple((OPEN, alt((parameterAlias, keyPropertyValue)), CLOSE)))));
fn simpleKey_wip<'a>(input: Input<'a>, arg: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, key) = delimited(
        OPEN,
        alt((
            keyPropertyValue_wip,
            map_opt(parameterAlias_wip, |n| input.parser.scope.resolve(n.name))
        )),
        CLOSE
    )(input.clone())?;

    //FIXME typecheck we are actually operating on a keyed resource
    Ok((input.clone(), Rc::new(input.parser.expr(ExprKind::Key(arg.clone(), vec![key])))))
}
//* compoundKey      = OPEN keyValuePair *( COMMA keyValuePair ) CLOSE
named!(compoundKey<Input, Input, Error>, call!(recognize(tuple((OPEN, keyValuePair, many0(tuple((COMMA, keyValuePair))), CLOSE)))));
fn compoundKey_wip<'a>(input: Input<'a>, arg: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, key) = delimited(OPEN, separated_nonempty_list(COMMA, |i| keyValuePair_wip(i, arg)), CLOSE)(input)?;

    Ok((input.clone(), Rc::new(input.parser.expr(ExprKind::Key(arg.clone(), key)))))
}

//* keyValuePair     = ( primitiveKeyProperty / keyPropertyAlias  ) EQ ( parameterAlias / keyPropertyValue )
named!(keyValuePair<Input, Input, Error>, call!(recognize(tuple((alt((primitiveKeyProperty, keyPropertyAlias)), EQ, alt((parameterAlias, keyPropertyValue)))))));
fn keyValuePair_wip<'a>(input: Input<'a>, child: &Rc<Expr>) -> ExprOutput<'a> {
    Err(Err::Error(()))
    // let (input, property) = map_opt(alt((primitiveKeyProperty, keyPropertyAlias)), |n| props.get(n))(input)?;
    // let (input, value) = preceded(EQ, alt((map(parameterAlias_wip, ast::KeyValue::ParameterAlias), |i| keyPropertyValue_wip(i, property))))(input)?;

    // Ok((input, ast::KeyProperty{property, value}))
}
//* keyPropertyValue = primitiveLiteral
named!(keyPropertyValue<Input, Input, Error>, call!(recognize(primitiveLiteral)));
// FIXME validate the primitive against the property type
fn keyPropertyValue_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    expr(map(|i| primitiveLiteral_wip(i), ExprKind::Lit))(input)
}
//* keyPropertyAlias = odataIdentifier
named!(keyPropertyAlias<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* keyPathSegments  = 1*( "/" keyPathLiteral )
named!(keyPathSegments<Input, Input, Error>, call!(recognize(many1(tuple((tag("/"), keyPathLiteral))))));
fn keyPathSegments_wip<'a>(input: Input<'a>, child: &Rc<Expr>) -> ExprOutput<'a> {
    Err(Err::Error(()))
    // let mut result = vec![];

    // for name in key {
    // 	let (i, value) = preceded(tag("/"), |i| keyPathLiteral_wip(i, props.get(name).unwrap()))(input)?;
    // 	input = i;

    // 	result.push(value);
    // }

    // Ok((input, ast::KeyPredicate{values: result}))
}
//* keyPathLiteral   = *pchar
// FIXME This rule is overly generic. It should be matching the primitive value that the particular
// property has
named!(keyPathLiteral<Input, Input, Error>, call!(recognize(many0(pchar))));
fn keyPathLiteral_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    unimplemented!()
    // map(recognize(many0(pchar)), |n: Input| ast::KeyProperty{property, value: ast::KeyValue::Value(n.data)})(input)
}
//*
//* singleNavigation = [ "/" qualifiedEntityTypeName ]
//*                    [ "/" propertyPath
//*                    / boundOperation
//*                    / ref
//*                    / value  ; request the media resource of a media entity
//*                    ]
named!(singleNavigation<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(alt((recognize(tuple((tag("/"), propertyPath)))
														   , boundOperation
														   , _ref
														   , _value
														   ))))))));
fn singleNavigation_wip<'a>(
    input: Input<'a>,
    kind: &'a schema::ty::Entity<'a>,
) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    //FIXME
    let (input, cast) = opt(value(
        ast::PathSegment::Cast,
        preceded(tag("/"), qualifiedEntityTypeName),
    ))(input)?;
    let (input, path) = opt(alt((
        preceded(tag("/"), |i| {
            propertyPath_wip(i, &kind.structural_props, &kind.navigation_props)
        }),
        |i| boundOperation_wip(i),
        map(_ref_wip, |s| vec![s]),
        map(_value_wip, |s| vec![s]),
    )))(input)?;

    let mut result = vec![];
    if let Some(cast) = cast {
        result.push(cast);
    }
    if let Some(mut path) = path {
        result.append(&mut path);
    }

    Ok((input, result))
}
//*
//* propertyPath = entityColNavigationProperty [ collectionNavigation ]
//*              / entityNavigationProperty    [ singleNavigation ]
//*              / complexColProperty          [ complexColPath ]
//*              / complexProperty             [ complexPath ]
//*              / primitiveColProperty        [ primitiveColPath ]
//*              / primitiveProperty           [ primitivePath ]
//*              / streamProperty              [ boundOperation ]
named!(propertyPath<Input, Input, Error>, call!(recognize(alt((tuple((entityColNavigationProperty, opt(collectionNavigation)))
						 , tuple((entityNavigationProperty, opt(singleNavigation)))
						 , tuple((complexColProperty, opt(complexColPath)))
						 , tuple((complexProperty, opt(complexPath)))
						 , tuple((primitiveColProperty, opt(primitiveColPath)))
						 , tuple((primitiveProperty, opt(primitivePath)))
						 , tuple((streamProperty, opt(boundOperation)))
						 )))));
fn propertyPath_wip<'a>(
    input: Input<'a>,
    structural: &'a HashMap<schema::Identifier, schema::property::Structural<'a>>,
    navigation: &'a HashMap<schema::Identifier, schema::property::Navigation<'a>>,
) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    unimplemented!();
    // use schema::ty;
    // use schema::ty::Ty;
    // use schema::property::*;

    // let (input, property) = map_opt(odataIdentifier, |name| properties.get(name.data))(input)?;

    // let (input, path) = match property.borrow() {
    // 	PropertyRef::Navigation(Navigation{ty, collection: true, ..}) => unimplemented!(), //opt(|i| collectionNavigation_wip(i, kind))(input)?,
    // 	PropertyRef::Navigation(Navigation{ty, collection: false, ..}) => opt(|i| singleNavigation_wip(i, ty))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Complex(kind), collection: true, ..}) => opt(|i| complexColPath_wip(i, &kind))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Complex(kind), collection: false, ..}) => opt(|i| complexPath_wip(i, &kind))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Primitive(ty::Primitive::Stream), ..}) => opt(|i| boundOperation_wip(i))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Primitive(_), collection: true, ..}) => opt(|i| primitiveColPath_wip(i))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Primitive(_), collection: false, ..}) => opt(|i| primitivePath_wip(i))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Enum(_), collection: true, ..}) => opt(|i| primitiveColPath_wip(i))(input)?,
    // 	PropertyRef::Structural(Structural{ty: Ty::Enum(_), collection: false, ..}) => opt(|i| primitivePath_wip(i))(input)?,
    // };

    // let mut result = vec![ast::PathSegment::Property(property)];
    // if let Some(mut path) = path {
    // 	result.append(&mut path);
    // }

    // Ok((input, result))
}

//*
//* primitiveColPath = count / boundOperation / ordinalIndex
named!(primitiveColPath<Input, Input, Error>, call!(alt((count, boundOperation, ordinalIndex))));
fn primitiveColPath_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    alt((
        map(count_wip, |c| vec![c]),
        |i| boundOperation_wip(i),
        map(|i| ordinalIndex_wip(i), |index| vec![index]),
    ))(input)
}

//*
//* primitivePath  = value / boundOperation
named!(primitivePath<Input, Input, Error>, call!(alt((_value, boundOperation))));
fn primitivePath_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    alt((map(_value_wip, |c| vec![c]), |i| boundOperation_wip(i)))(input)
}
//*
//* complexColPath = ordinalIndex
//*                / [ "/" qualifiedComplexTypeName ] [ count / boundOperation ]
//  errata: The ABNF doesn't allow selecting a specific element and then continuing with a complexPath
//  rule. Is this a mistake?
named!(complexColPath<Input, Input, Error>, call!(alt((ordinalIndex, recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(alt((count, boundOperation))))))))));
fn complexColPath_wip<'a>(
    input: Input<'a>,
    kind: &'a schema::ty::Complex,
) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    alt(
        (map(|i| ordinalIndex_wip(i), |index| vec![index]), |input| {
            let (input, cast) = opt(value(
                ast::PathSegment::Cast,
                preceded(tag("/"), qualifiedComplexTypeName),
            ))(input)?;
            let (input, path) = opt(alt((map(count_wip, |s| vec![s]), |i| {
                boundOperation_wip(i)
            })))(input)?;

            let mut result = vec![];
            if let Some(cast) = cast {
                result.push(cast);
            }
            if let Some(mut path) = path {
                result.append(&mut path);
            }
            Ok((input, result))
        }),
    )(input)
}
//*
//* complexPath    = [ "/" qualifiedComplexTypeName ]
//*                  [ "/" propertyPath
//*                  / boundOperation
//*                  ]
named!(complexPath<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(alt((recognize(tuple((tag("/"), propertyPath))), boundOperation))))))));
fn complexPath_wip<'a>(
    input: Input<'a>,
    kind: &'a schema::ty::Complex<'a>,
) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    let (input, cast) = opt(value(
        ast::PathSegment::Cast,
        preceded(tag("/"), qualifiedComplexTypeName),
    ))(input)?;
    let (input, path) = opt(alt((
        preceded(tag("/"), |i| {
            propertyPath_wip(i, &kind.structural_props, &kind.navigation_props)
        }),
        |i| boundOperation_wip(i),
    )))(input)?;

    let mut result = vec![];
    if let Some(cast) = cast {
        result.push(cast);
    }
    if let Some(mut path) = path {
        result.append(&mut path);
    }
    Ok((input, result))
}
//*
//* filterInPath = '/$filter' EQ parameterAlias
named!(filterInPath<Input, Input, Error>, call!(recognize(tuple((tag("/$filter"), EQ, parameterAlias)))));
fn filterInPath_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, param) = map_opt(
        preceded(tuple((tag("/$filter"), EQ)), parameterAlias_wip2),
        |param| {
            input.parser.unparsed_params.borrow_mut().remove(param.data)
        },
    )(input.clone())?;

    let param_input = Input{
        parser: input.parser,
        data: param,
    };

    param_input.parser.scope.push_frame();
    param_input.parser.scope.push("$it", child.clone());
    param_input.parser.scope.push("$this", child.clone());
    let (_, expr) = all_consuming(|i| commonExpr_wip(i, 0))(param_input.clone())?;
    param_input.parser.scope.pop_frame();

    Ok((input.clone(), Rc::new(input.parser.expr(ExprKind::Filter(Rc::clone(child), expr)))))
}
//*
//* each  = '/$each'
named!(each<Input, Input, Error>, call!(tag("/$each")));
named!(each_wip<Input, ast::PathSegment, Error>, call!(value(ast::PathSegment::Each, tag("/$each"))));
fn each_wip2<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    //FIXME typecheck that input type is a collection
    // typecheck::<ast::ty::Collection>(child)?
    expr(map(tag("/$each"), |_| ExprKind::Each(Rc::clone(child))))(input)
}
//* count = '/$count'
named!(count<Input, Input, Error>, call!(tag("/$count")));
named!(count_wip<Input, ast::PathSegment, Error>, call!(value(ast::PathSegment::Count, tag("/$count"))));
fn count_wip2<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    //FIXME typecheck that input type is a collection
    // typecheck::<ast::ty::Collection>(child)?
    expr(map(tag("/$count"), |_| ExprKind::Count(Rc::clone(child))))(input)
}
//* ref   = '/$ref'
named!(_ref<Input, Input, Error>, call!(tag("/$ref")));
named!(_ref_wip<Input, ast::PathSegment, Error>, call!(value(ast::PathSegment::Ref, tag("/$ref"))));
fn _ref_wip2<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    //FIXME typecheck that input type is a collection
    expr(map(tag("/$ref"), |_| ExprKind::Ref(Rc::clone(child))))(input)
}
//* value = '/$value'
named!(_value<Input, Input, Error>, call!(tag("/$value")));
named!(_value_wip<Input, ast::PathSegment, Error>, call!(value(ast::PathSegment::Value, tag("/$value"))));
//*
//* ordinalIndex = "/" 1*DIGIT
//  errata: Even though the ABNF encodes only positive integers, the OData spec defines negative ordinal
//  indices too. See:
//  http://docs.oasis-open.org/odata/odata/v4.01/cs01/part1-protocol/odata-v4.01-cs01-part1-protocol.html#sec_RequestinganIndividualMemberofanOrde
named!(ordinalIndex<Input, Input, Error>, call!(recognize(tuple((tag("/"), many1(DIGIT))))));
fn ordinalIndex_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, ast::PathSegment<'a>, Error> {
    map(
        preceded(
            tag("/"),
            map_res(recognize(tuple((opt(tag("-")), digit1))), |n: Input| {
                n.data.parse()
            }),
        ),
        ast::PathSegment::OrdinalIndex,
    )(input)
}
//*
//* ; boundOperation segments can only be composed if the type of the previous segment
//* ; matches the type of the first parameter of the action or function being called.
//* ; Note that the rule name reflects the return type of the function.
//* boundOperation = "/" ( boundActionCall
//*                      / boundEntityColFunctionCall    [ collectionNavigation ]
//*                      / boundEntityFunctionCall       [ singleNavigation ]
//*                      / boundComplexColFunctionCall   [ complexColPath ]
//*                      / boundComplexFunctionCall      [ complexPath ]
//*                      / boundPrimitiveColFunctionCall [ primitiveColPath ]
//*                      / boundPrimitiveFunctionCall    [ primitivePath ]
//*                      / boundFunctionCallNoParens
//*                      )
named!(boundOperation<Input, Input, Error>, call!(recognize(tuple((tag("/"), alt((boundActionCall
								     , recognize(tuple((boundEntityColFunctionCall, opt(collectionNavigation))))
								     , recognize(tuple((boundEntityFunctionCall, opt(singleNavigation))))
								     , recognize(tuple((boundComplexColFunctionCall, opt(complexColPath))))
								     , recognize(tuple((boundComplexFunctionCall, opt(complexPath))))
								     , recognize(tuple((boundPrimitiveColFunctionCall, opt(primitiveColPath))))
								     , recognize(tuple((boundPrimitiveFunctionCall, opt(primitivePath))))
								     , boundFunctionCallNoParens
								     )))))));
fn boundOperation_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    unimplemented!()
}
fn boundOperation_wip2<'a>(input: Input<'a>, arg: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    Err(Err::Error(()))
    // let (input, bound_op) = preceded(
    //     tag("/"),
    //     alt((
    //         |i| boundActionCall_wip(i, arg),
    //         |i| boundFunctionCall_wip(i, arg)
    //     ))
    // )(input)?;

    // let (input, nav) = match &bound_op.node {
    //     ExprKind::Function(f, _) => (input, None)
    //     // match (f.return_type.ty, f.return_type.collection) {
    //     //     (schema::ty::Ty::Entity(_), true) => opt(|i| collectionNavigation_wip(i, &bound_op))(input)?;
    //     //     (schema::ty::Ty::Entity(_), false) => opt(|i| singleNavigation_wip(i, &bound_op))(input)?;
    //     //     (schema::ty::Ty::Complex(_), true) => opt(|i| complexColPath_wip(i, &bound_op))(input)?;
    //     //     (schema::ty::Ty::Complex(_), false) => opt(|i| complexPath_wip(i, &bound_op))(input)?;
    //     //     (schema::ty::Ty::Primitive(_), true) => opt(|i| primitiveColPath_wip(i, &bound_op))(input)?;
    //     //     (schema::ty::Ty::Primitive(_), false) => opt(|i| primitivePath_wip(i, &bound_op))(input)?;
    //     // },
    //     ExprKind::Action(a, _) => (input, None),
    // };

    // Ok((input, nav.unwrap_or(bound_op)))
}

//*
//* actionImportCall = actionImport
named!(actionImportCall<Input, Input, Error>, call!(recognize(actionImport)));
//* boundActionCall  = namespace "." action
//*                    ; with the added restriction that the binding parameter MUST be either an entity or collection of entities
//*                    ; and is specified by reference using the URI immediately preceding (to the left) of the boundActionCall
named!(boundActionCall<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), action)))));
fn boundActionCall_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, (namespace, item)) = namespaced_item(input)?;

    input.parser.document.schemas
        .get(namespace)
        //FIXME check if the first parameter is the same type as the current child
        .and_then(|s| s.actions.get(item).filter(|a| a.is_bound))
        .map(|a| (input.clone(), Rc::new(input.parser.expr(ExprKind::Action(a, vec![Rc::clone(child)])))))
        .ok_or(Err::Error(()))
}
//*
//* ; The following boundXxxFunctionCall rules have the added restrictions that
//* ;  - the function MUST support binding, and
//* ;  - the binding parameter type MUST match the type of resource identified by the
//* ;    URI immediately preceding (to the left) of the boundXxxFunctionCall, and
//* ;  - the functionParameters MUST NOT include the bindingParameter.
//TODO(validate)
//* boundEntityFunctionCall       = namespace "." entityFunction       functionParameters
named!(boundEntityFunctionCall<Input, Input, Error>, call!(recognize(tuple((entityFunction, functionParameters)))));
//* boundEntityColFunctionCall    = namespace "." entityColFunction    functionParameters
named!(boundEntityColFunctionCall<Input, Input, Error>, call!(recognize(tuple((entityColFunction, functionParameters)))));
//* boundComplexFunctionCall      = namespace "." complexFunction      functionParameters
named!(boundComplexFunctionCall<Input, Input, Error>, call!(recognize(tuple((complexFunction, functionParameters)))));
//* boundComplexColFunctionCall   = namespace "." complexColFunction   functionParameters
named!(boundComplexColFunctionCall<Input, Input, Error>, call!(recognize(tuple((complexColFunction, functionParameters)))));
//* boundPrimitiveFunctionCall    = namespace "." primitiveFunction    functionParameters
named!(boundPrimitiveFunctionCall<Input, Input, Error>, call!(recognize(tuple((primitiveFunction, functionParameters)))));
//* boundPrimitiveColFunctionCall = namespace "." primitiveColFunction functionParameters
named!(boundPrimitiveColFunctionCall<Input, Input, Error>, call!(recognize(tuple((primitiveColFunction, functionParameters)))));
//*
//* boundFunctionCallNoParens     = namespace "." entityFunction
//*                               / namespace "." entityColFunction
//*                               / namespace "." complexFunction
//*                               / namespace "." complexColFunction
//*                               / namespace "." primitiveFunction
//*                               / namespace "." primitiveColFunction
named!(boundFunctionCallNoParens<Input, Input, Error>, call!(recognize(alt((tuple((namespace, tag("."), entityFunction))
							      , tuple((namespace, tag("."), entityColFunction))
						   	      , tuple((namespace, tag("."), complexFunction))
						   	      , tuple((namespace, tag("."), complexColFunction))
						   	      , tuple((namespace, tag("."), primitiveFunction))
						   	      , tuple((namespace, tag("."), primitiveColFunction))
							 )))));
fn boundFunctionCall_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, (namespace, item)) = namespaced_item(input)?;

    let (input, function) = input.parser.document.schemas
        .get(namespace)
        //FIXME check if the first parameter is the same type as the current child
        .and_then(|s| s.functions.get(item).filter(|f| f.is_bound))
        .map(|f| (input.clone(), Rc::new(input.parser.expr(ExprKind::Function(&f, vec![Rc::clone(child)])))))
        .ok_or(Err::Error(()))?;

    let (input, args) = opt(|i| functionParameters_wip(i, &function))(input)?;

    Ok((input, args.unwrap_or(function)))
}

//*
//* entityFunctionImportCall       = entityFunctionImport       functionParameters
named!(entityFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((entityFunctionImport,functionParameters)))));
//* entityColFunctionImportCall    = entityColFunctionImport    functionParameters
named!(entityColFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((entityColFunctionImport,functionParameters)))));
//* complexFunctionImportCall      = complexFunctionImport      functionParameters
named!(complexFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((complexFunctionImport,functionParameters)))));
//* complexColFunctionImportCall   = complexColFunctionImport   functionParameters
named!(complexColFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((complexColFunctionImport,functionParameters)))));
//* primitiveFunctionImportCall    = primitiveFunctionImport    functionParameters
named!(primitiveFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((primitiveFunctionImport,functionParameters)))));
//* primitiveColFunctionImportCall = primitiveColFunctionImport functionParameters
named!(primitiveColFunctionImportCall<Input, Input, Error>, call!(recognize(tuple((primitiveColFunctionImport,functionParameters)))));
//*
//* functionImportCallNoParens     = entityFunctionImport
//*                                / entityColFunctionImport
//*                                / complexFunctionImport
//*                                / complexColFunctionImport
//*                                / primitiveFunctionImport
//*                                / primitiveColFunctionImport
named!(functionImportCallNoParens<Input, Input, Error>, call!(alt((entityFunctionImport
						    , entityColFunctionImport
						    , complexFunctionImport
						    , complexColFunctionImport
						    , primitiveFunctionImport
						    , primitiveColFunctionImport))));
//*
//* functionParameters = OPEN [ functionParameter *( COMMA functionParameter ) ] CLOSE
named!(functionParameters<Input, Input, Error>, call!(recognize(tuple((OPEN, opt(tuple((functionParameter, many0(tuple((COMMA, functionParameter)))))), CLOSE)))));
fn functionParameters_wip<'a>(input: Input<'a>, function: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, params) = delimited(OPEN, separated_list(COMMA, functionParameter), CLOSE)(input)?;
    unimplemented!()
}
//* functionParameter  = parameterName EQ ( parameterAlias / primitiveLiteral )
named!(functionParameter<Input, Input, Error>, call!(recognize(tuple((parameterName, EQ, alt((parameterAlias, primitiveLiteral)))))));
//* parameterName      = odataIdentifier
named!(parameterName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* parameterAlias     = AT odataIdentifier
named!(parameterAlias<Input, Input, Error>, call!(preceded(AT, odataIdentifier)));
named!(parameterAlias_wip<Input, ast::ParameterAlias, Error>, call!(map(preceded(AT, odataIdentifier), |name| ast::ParameterAlias{name: name.data})));
fn parameterAlias_wip2<'a>(input: Input<'a>) -> IResult<Input<'a>, Input<'a>, Error>  {
    recognize(preceded(AT, odataIdentifier))(input)
}
//*
//* crossjoin = '$crossjoin' OPEN
//*             entitySetName *( COMMA entitySetName )
//*             CLOSE
named!(crossjoin<Input, Input, Error>, call!(recognize(tuple((tag("$crossjoin"), OPEN, entitySetName, many0(tuple((COMMA, entitySetName))), CLOSE)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 2. Query Options
//* ;------------------------------------------------------------------------------
//*
//* queryOptions = queryOption *( "&" queryOption )
named!(queryOptions<Input, Input, Error>, call!(recognize(tuple((queryOption, many0(tuple((tag("&"), queryOption))))))));
fn queryOptions_wip<'a>(input: Input<'a>, mut root: ast::ResourceQuery<'a>) -> IResult<Input<'a>, ast::ResourceQuery<'a>, Error> {
    // FIXME we have to parse $compute first
    for option in input.data.split('&') {
        if let Some(idx) = option.find('=') {
            let (key, value) = option.split_at(idx);
            let key = Input {
                parser: input.parser,
                data: key,
            };
            if let Err(_) = parameterAlias_wip2(key) {
                let option_input = Input{
                    parser: input.parser,
                    data: option,
                };

                let (_, option) = queryOption_wip(option_input)?;

                match option {
                    QueryOption::Select(v) => root.expr.select = v,
                    QueryOption::Expand(v) => root.expr.expand = v,
                    QueryOption::Filter(e) => root.expr.filter = Some(e),
                    QueryOption::Search(e) => root.expr.search = Some(e),
                    QueryOption::Orderby(v) => root.expr.orderby = v,
                    QueryOption::Skip(n) => root.expr.skip = Some(n),
                    QueryOption::Top(n) => root.expr.top = Some(n),
                    QueryOption::Count(c) => root.expr.count = c,
                    QueryOption::Levels(n) => root.expr.levels = Some(n),
                    QueryOption::Compute(v) => root.expr.compute = v,
                    QueryOption::Params(v) => root.expr.params = v,
                    QueryOption::Deltatoken(s) => root.delta_token = Some(s),
                    QueryOption::Format(s) => root.format = Some(s),
                    QueryOption::Id(s) => root.id = Some(s),
                    QueryOption::Schemaversion(s) => root.schema_version = Some(s),
                    QueryOption::Skiptoken(s) => root.skip_token = Some(s),
                    QueryOption::Index(n) => root.index = Some(n),
                }
            }
        }
    }
    Ok((input.slice(input.input_len()..), root))
}
//* queryOption  = systemQueryOption
//*              / aliasAndValue
//*              / nameAndValue
//*              / customQueryOption
named!(queryOption<Input, Input, Error>, call!(alt((systemQueryOption, aliasAndValue, nameAndValue, customQueryOption))));
fn queryOption_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, QueryOption<'a>, Error> {
    alt((
        // XXX we have to be careful here. A malformed $filter for example would fail the first
        // parser and fall through to the custom query option one which will match anything,
        // leading to running the wrong query. We have to cut() on error after we successfully
        // matched the prefix of a system query option
        |i| systemQueryOption_wip(i),
        |i| unimplemented!(),
        // value(ast::QueryOption::Alias, aliasAndValue),
        // value(ast::QueryOption::Name, nameAndValue), // this is the implicit parameter alias case in the protocol
        // customQueryOption_wip,
    ))(input)
}
//*
//* batchOptions = batchOption *( "&" batchOption )
named!(batchOptions<Input, Vec<ast::QueryOption>, Error>, call!(separated_nonempty_list(tag("&"), batchOption)));
//* batchOption  = format
//*              /customQueryOption
named!(batchOption<Input, ast::QueryOption, Error>, call!(alt((format_wip, customQueryOption_wip))));
//*
//* metadataOptions = metadataOption *( "&" metadataOption )
named!(metadataOptions<Input, Input, Error>, call!(recognize(tuple((metadataOption, many0(tuple((tag("&"), metadataOption))))))));
//* metadataOption  = format
//*                 /customQueryOption
named!(metadataOption<Input, Input, Error>, call!(alt((format, customQueryOption))));
//*
//* entityOptions  = *( entityIdOption "&" ) id *( "&" entityIdOption )
named!(entityOptions<Input, Input, Error>, call!(recognize(tuple((many0(tuple((entityIdOption, tag("&")))), id, many0(tuple((tag("&"), entityIdOption))))))));
//* entityIdOption = format
//*                / customQueryOption
named!(entityIdOption<Input, Input, Error>, call!(alt((format, customQueryOption))));
//* entityCastOptions = *( entityCastOption "&" ) id *( "&" entityCastOption )
named!(entityCastOptions<Input, Input, Error>, call!(recognize(tuple((many0(tuple((entityCastOption, tag("&")))), id, many0(tuple((tag("&"), entityCastOption))))))));
//* entityCastOption  = entityIdOption
//*                   / expand
//*                   / select
named!(entityCastOption<Input, Input, Error>, call!(alt((entityIdOption, expand, select))));
//*
//* id = ( "$id" / "id" ) EQ IRI-in-query
named!(id<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$id"), tag_no_case("id"))), EQ, IRI_in_query)))));
//*
//* systemQueryOption = compute
//*                   / deltatoken
//*                   / expand
//*                   / filter
//*                   / format
//*                   / id
//*                   / inlinecount
//*                   / orderby
//*                   / schemaversion
//*                   / search
//*                   / select
//*                   / skip
//*                   / skiptoken
//*                   / top
//*                   / index
named!(systemQueryOption<Input, Input, Error>, call!(alt((compute
					   , deltatoken
					   , expand
					   , filter
					   , format
					   , id
					   , inlinecount
					   , orderby
					   , schemaversion
					   , search
					   , select
					   , skip
					   , skiptoken
					   , top
					   , index))));
fn systemQueryOption_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, QueryOption<'a>, Error> {
    alt((
        // value(QueryOption::Compute, compute),
        // value(QueryOption::DeltaToken, deltatoken),
        // value(QueryOption::Expand, expand),
        |i| filter_wip(i),
        |i| filter_wip(i),
        // format_wip,
        // value(QueryOption::Id, id),
        // value(QueryOption::Count, inlinecount),
        // value(QueryOption::OrderBy, orderby),
        // value(QueryOption::SchemaVersion, schemaversion),
        // value(QueryOption::Search, search),
        // value(QueryOption::Select, select),
        // value(QueryOption::Skip, skip),
        // value(QueryOption::SkipToken, skiptoken),
        // value(QueryOption::Top, top),
        // value(QueryOption::Index, index),
    ))(input)
}

//*
//* compute          = ( "$compute" / "compute" ) EQ computeItem *( COMMA computeItem )
named!(compute<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$compute"), tag_no_case("compute"))), EQ, computeItem, many0(tuple((COMMA, computeItem))))))));
//* computeItem      = commonExpr RWS "as" RWS computedProperty
named!(computeItem<Input, Input, Error>, call!(recognize(tuple((commonExpr, RWS, tag_no_case("as"), RWS, computedProperty)))));
//* computedProperty = odataIdentifier
named!(computedProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* expand            = ( "$expand" / "expand" ) EQ expandItem *( COMMA expandItem )
named!(expand<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$expand"), tag_no_case("expand"))), EQ, expandItem, many0(tuple((COMMA, expandItem))))))));
//* expandItem        = STAR [ ref / OPEN levels CLOSE ]
//*                   / "$value"
//*                   / expandPath
//*                     [ ref   [ OPEN expandRefOption   *( SEMI expandRefOption   ) CLOSE ]
//*                     / count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                     /         OPEN expandOption      *( SEMI expandOption      ) CLOSE
//*                     ]
named!(expandItem<Input, Input, Error>, call!(recognize(alt((recognize(tuple((STAR, opt(alt((_ref, recognize(tuple((OPEN, levels, CLOSE)))))))))
					       , tag_no_case("$value")
					       , recognize(tuple((expandPath,
								   opt(alt((
									     recognize(tuple((_ref, opt(tuple((OPEN, expandRefOption, many0(tuple((SEMI, expandRefOption))), CLOSE))))))
									     , recognize(tuple((count, opt(tuple((OPEN, expandCountOption, many0(tuple((SEMI, expandCountOption))), CLOSE))))))
									     , recognize(tuple((OPEN, expandOption, many0(tuple((SEMI, expandOption))), CLOSE)))
									 )))
								  )))
						)))));

//* expandPath        = [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                     *( ( complexProperty / complexColProperty ) "/" [ qualifiedComplexTypeName "/" ] )
//*                     ( STAR / streamProperty / navigationProperty [ "/" qualifiedEntityTypeName ] )
named!(expandPath<Input, Input, Error>, call!(recognize(tuple((
						opt(tuple((alt((qualifiedEntityTypeName, qualifiedComplexTypeName)), tag("/")))),
						many0(tuple((alt((complexProperty, complexColProperty)), tag("/"), opt(tuple((qualifiedComplexTypeName, tag("/"))))))),
						alt((STAR, streamProperty, recognize(tuple((navigationProperty, opt(tuple((tag("/"), qualifiedEntityTypeName))))))))
						)))));

//* expandCountOption = filter
//*                   / search
named!(expandCountOption<Input, Input, Error>, call!(alt((filter, search))));
//* expandRefOption   = expandCountOption
//*                   / orderby
//*                   / skip
//*                   / top
//*                   / inlinecount
named!(expandRefOption<Input, Input, Error>, call!(alt((expandCountOption, orderby, skip, top, inlinecount))));
//* expandOption      = expandRefOption
//*                   / select
//*                   / expand
//*                   / compute
//*                   / levels
//*                   / aliasAndValue
named!(expandOption<Input, Input, Error>, call!(alt((expandRefOption, select, expand, compute, levels, aliasAndValue))));
//*
//* levels = ( "$levels" / "levels" ) EQ ( oneToNine *DIGIT / "max" )
named!(levels<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$levels"), tag_no_case("levels"))), EQ, alt((recognize(tuple((oneToNine, many0(DIGIT)))), tag_no_case("max"))))))));
//*
//* filter = ( "$filter" / "filter" ) EQ boolCommonExpr
named!(filter<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$filter"), tag_no_case("filter"))), EQ, boolCommonExpr)))));
fn filter_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, QueryOption<'a>, Error> {
    let (input, _) = tuple((opt(tag("$")), tag_no_case("filter"), EQ))(input)?;

    // Any errors after this point are fatal
    cut(map(|i| commonExpr_wip(i, 0), QueryOption::Filter))(input)
}
//*
//* orderby     = ( "$orderby" / "orderby" ) EQ orderbyItem *( COMMA orderbyItem )
named!(orderby<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$orderby"), tag_no_case("orderby"))), EQ, orderbyItem, many0(tuple((COMMA, orderbyItem))))))));
//* orderbyItem = commonExpr [ RWS ( "asc" / "desc" ) ]
named!(orderbyItem<Input, Input, Error>, call!(recognize(tuple((commonExpr, opt(tuple((RWS, alt((tag_no_case("asc"), tag_no_case("desc")))))))))));
//*
//* skip = ( "$skip" / "skip" ) EQ 1*DIGIT
named!(skip<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$skip"), tag_no_case("skip"))), EQ, many1(DIGIT))))));
//* top  = ( "$top"  / "top"  ) EQ 1*DIGIT
named!(top<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$top"), tag_no_case("top"))), EQ, many1(DIGIT))))));
//*
//* index  = ( "$index" / "index" ) EQ 1*DIGIT
named!(index<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$index"), tag_no_case("index"))), EQ, many1(DIGIT))))));
//*
//* format = ( "$format" / "format" ) EQ
//*          ( "atom"
//*          / "json"
//*          / "xml"
//*          / 1*pchar "/" 1*pchar ; <a data service specific value indicating a
//*          )                     ; format specific to the specific data service> or
//*                                ; <An IANA-defined [IANA-MMT] content type>
// errata: pchar includes special characters like & and =. It should probably be something like qchar_no_AMP_EQ
named!(format_wip<Input, ast::QueryOption, Error>, call!(preceded(
					     tuple((opt(tag("$")), tag_no_case("format"), EQ)),
					     map(
						alt((
							     nom::combinator::value(ast::FormatKind::Atom, tag_no_case("atom"))
							   , nom::combinator::value(ast::FormatKind::JSON, tag_no_case("json"))
							   , nom::combinator::value(ast::FormatKind::XML, tag_no_case("xml"))
							   , map(recognize(tuple((many1(pchar), tag("/"), many1(pchar)))), |res| ast::FormatKind::Custom(res.data))
						)),
						ast::QueryOption::Format
					     ))));
named!(format<Input, Input, Error>, call!(recognize(format_wip)));

//*
//* inlinecount = ( "$count" / "count" ) EQ booleanValue
named!(inlinecount<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$count"), tag_no_case("count"))), EQ, booleanValue)))));
//*
//* schemaversion   = ( "$schemaversion" / "schemaversion" ) EQ ( STAR / 1*unreserved )
named!(schemaversion<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$schemaversion"), tag_no_case("schemaversion"))), EQ, alt((STAR, recognize(many1(unreserved)))))))));
//*
//* search     = ( "$search" / "search" ) EQ BWS searchExpr
named!(search<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$search"), tag_no_case("search"))), EQ, BWS, searchExpr)))));
//* searchExpr = ( OPEN BWS searchExpr BWS CLOSE
//*              / searchTerm
//*              ) [ searchOrExpr
//*                / searchAndExpr
//*                ]
named!(searchExpr<Input, Input, Error>, call!(recognize(tuple((alt((recognize(tuple((OPEN, BWS, searchExpr, BWS, CLOSE))), searchTerm)), opt(alt((searchOrExpr, searchAndExpr))))))));
//*
//* searchOrExpr  = RWS 'OR'  RWS searchExpr
named!(searchOrExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag("OR"), RWS, searchExpr)))));
//* searchAndExpr = RWS [ 'AND' RWS ] searchExpr
named!(searchAndExpr<Input, Input, Error>, call!(recognize(tuple((RWS, opt(tuple((tag("AND"), RWS))), searchExpr)))));
//*
//* searchTerm   = [ 'NOT' RWS ] ( searchPhrase / searchWord )
named!(searchTerm<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("NOT"), RWS))), alt((searchPhrase, searchWord)))))));
//* searchPhrase = quotation-mark 1*qchar-no-AMP-DQUOTE quotation-mark
named!(searchPhrase<Input, Input, Error>, call!(recognize(tuple((quotation_mark, many1(qchar_no_AMP_DQUOTE), quotation_mark)))));
//*
//* ; A searchWord is a sequence of one or more letters, digits, commas, or dots.
//* ; This includes Unicode characters of categories L or N using UTF-8 and percent-encoding.
//* ; The words AND, OR, and NOT are not a valid searchWord.
//* ; Expressing this in ABNF is somewhat clumsy, so the following rule is overly generous.
//TODO(validation)
//* searchWord   = 1*( ALPHA / DIGIT / COMMA / "." / pct-encoded )
named!(searchWord<Input, Input, Error>, call!(recognize(many1(alt((ALPHA, DIGIT, COMMA, tag("."), pct_encoded))))));
//*
//* select         = ( "$select" / "select" ) EQ selectItem *( COMMA selectItem )
named!(select<Input, Input, Error>, call!(recognize(tuple((alt((tag_no_case("$select"), tag_no_case("select"))), EQ, selectItem, many0(tuple((COMMA, selectItem))))))));
//* selectItem     = STAR
//*                / allOperationsInSchema
//*                / [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                  ( selectProperty
//*                  / qualifiedActionName
//*                  / qualifiedFunctionName
//*                  )
named!(selectItem<Input, Input, Error>, call!(alt((STAR
				    , allOperationsInSchema
				    , recognize(tuple((opt(tuple((alt((qualifiedEntityTypeName, qualifiedComplexTypeName)), tag("/")))),
							alt((selectProperty, qualifiedActionName, qualifiedFunctionName))
							)))
				   ))));
//* selectProperty = primitiveProperty
//*                / primitiveColProperty [ OPEN selectOptionPC *( SEMI selectOptionPC ) CLOSE ]
//*                / navigationProperty
//*                / selectPath [ OPEN selectOption *( SEMI selectOption ) CLOSE
//*                             / "/" selectProperty
//*                             ]
// errata: missing brackets around the OPEN selectOption... and around "/" selectProperty
named!(selectProperty<Input, Input, Error>, call!(alt((primitiveProperty
					, recognize(tuple((primitiveColProperty, opt(tuple((OPEN, selectOptionPC, many0(tuple((SEMI, selectOptionPC))), CLOSE))))))
					, navigationProperty
					, recognize(tuple((selectPath, opt(alt((
										recognize(tuple((OPEN, selectOption, many0(tuple((SEMI, selectOption))), CLOSE)))
										, recognize(tuple((tag("/"), selectProperty)))
										))))))
					))));
//* selectPath     = ( complexProperty / complexColProperty ) [ "/" qualifiedComplexTypeName ]
named!(selectPath<Input, Input, Error>, call!(recognize(tuple((alt((complexProperty, complexColProperty)), opt(tuple((tag("/"), qualifiedComplexTypeName))))))));
//* selectOptionPC = filter / search / inlinecount / orderby / skip / top
named!(selectOptionPC<Input, Input, Error>, call!(alt((filter, search, inlinecount, orderby, skip, top))));
//* selectOption   = selectOptionPC
//*                / compute / select / expand / aliasAndValue
named!(selectOption<Input, Input, Error>, call!(alt((selectOptionPC, compute, select, expand, aliasAndValue))));
//*
//* allOperationsInSchema = namespace "." STAR
named!(allOperationsInSchema<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), STAR)))));
//*
//* ; The parameterNames uniquely identify the bound function overload
//* ; only if it has overloads.
//* qualifiedActionName   = namespace "." action
named!(qualifiedActionName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), action)))));
//* qualifiedFunctionName = namespace "." function [ OPEN parameterNames CLOSE ]
named!(qualifiedFunctionName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), function, opt(tuple((OPEN, parameterNames, CLOSE))))))));
//*
//* ; The names of all non-binding parameters, separated by commas
//* parameterNames = parameterName *( COMMA parameterName )
named!(parameterNames<Input, Input, Error>, call!(recognize(tuple((parameterName, many0(tuple((COMMA, parameterName))))))));
//*
//* deltatoken = "$deltatoken" EQ 1*( qchar-no-AMP )
named!(deltatoken<Input, Input, Error>, call!(recognize(tuple((tag_no_case("$deltatoken"), EQ, many1(qchar_no_AMP))))));
//*
//* skiptoken = "$skiptoken" EQ 1*( qchar-no-AMP )
named!(skiptoken<Input, Input, Error>, call!(recognize(tuple((tag_no_case("$skiptoken"), EQ, many1(qchar_no_AMP))))));
//*
//* aliasAndValue = parameterAlias EQ parameterValue
named!(aliasAndValue<Input, Input, Error>, call!(recognize(tuple((parameterAlias, EQ, parameterValue)))));
//*
//* nameAndValue = parameterName EQ parameterValue
named!(nameAndValue<Input, Input, Error>, call!(recognize(tuple((parameterName, EQ, parameterValue)))));
//*
//* parameterValue = arrayOrObject
//*                / commonExpr
named!(parameterValue<Input, Input, Error>, call!(alt((arrayOrObject, commonExpr))));
//*
//* customQueryOption = customName [ EQ customValue ]
named!(customQueryOption_wip<Input, ast::QueryOption, Error>, do_parse!(foo: call!(recognize(tuple((customName, opt(tuple((EQ, customValue))))))) >> (ast::QueryOption::Custom(foo.data))));
named!(customQueryOption<Input, Input, Error>, call!(recognize(customQueryOption_wip)));
//* customName        = qchar-no-AMP-EQ-AT-DOLLAR *( qchar-no-AMP-EQ )
named!(customName<Input, Input, Error>, call!(recognize(tuple((qchar_no_AMP_EQ_AT_DOLLAR, many0(qchar_no_AMP_EQ))))));
//* customValue       = *( qchar-no-AMP )
named!(customValue<Input, Input, Error>, call!(recognize(many0(qchar_no_AMP))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 3. Context URL Fragments
//* ;------------------------------------------------------------------------------
//*
//* context         = "#" contextFragment
named!(context<Input, Input, Error>, call!(recognize(tuple((tag("#"), contextFragment)))));
//* contextFragment = 'Collection($ref)'
//*                 / '$ref'
//*                 / 'Collection(Edm.EntityType)'
//*                 / 'Collection(Edm.ComplexType)'
//*                 / singletonEntity [ navigation *( containmentNavigation ) [ "/" qualifiedEntityTypeName ] ] [ selectList ]
//*                 / qualifiedTypeName [ selectList ]
//*                 / entitySet ( '/$deletedEntity' / '/$link' / '/$deletedLink' )
//*                 / entitySet keyPredicate "/" contextPropertyPath [ selectList ]
//*                 / entitySet [ selectList ] [ '/$entity' / '/$delta' ]
named!(contextFragment<Input, Input, Error>, call!(alt((tag("Collection($ref)")
					 , tag("$ref")
					 , tag("Collection(Edm.EntityType)")
					 , tag("Collection(Edm.ComplexType)")
					 , recognize(tuple((singletonEntity,
							     opt(tuple((navigation,
									 many0(containmentNavigation),
									 opt(tuple((tag("/"), qualifiedEntityTypeName)))
							     ))),
							     opt(selectList)
					   )))
					 , recognize(tuple((qualifiedTypeName, opt(selectList))))
					 , recognize(tuple((entitySet, alt((tag("/$deletedEntity"), tag("/$link"), tag("/$deletedLink"))))))
					 , recognize(tuple((entitySet, keyPredicate, tag("/"), contextPropertyPath, opt(selectList))))
					 , recognize(tuple((entitySet, opt(selectList), opt(alt((tag("/$entity"), tag("/$delta")))))))
				    ))));
//*
//* entitySet = entitySetName *( containmentNavigation ) [ "/" qualifiedEntityTypeName ]
named!(entitySet<Input, Input, Error>, call!(recognize(tuple((entitySetName, many0(containmentNavigation), opt(tuple((tag("/"), qualifiedEntityTypeName))))))));
//*
//* containmentNavigation = keyPredicate [ "/" qualifiedEntityTypeName ] navigation
named!(containmentNavigation<Input, Input, Error>, call!(recognize(tuple((keyPredicate, opt(tuple((tag("/"), qualifiedEntityTypeName))), navigation)))));
//* navigation            = *( "/" complexProperty [ "/" qualifiedComplexTypeName ] ) "/" navigationProperty
named!(navigation<Input, Input, Error>, call!(recognize(tuple((many0(tuple((tag("/"), complexProperty, opt(tuple((tag("/"), qualifiedComplexTypeName)))))), tag("/"), navigationProperty)))));
//*
//* selectList         = OPEN selectListItem *( COMMA selectListItem ) CLOSE
named!(selectList<Input, Input, Error>, call!(recognize(tuple((OPEN, selectListItem, many0(tuple((COMMA, selectListItem))), CLOSE)))));
//* selectListItem     = STAR ; all structural properties
//*                    / allOperationsInSchema
//*                    / [ qualifiedEntityTypeName "/" ]
//*                      ( qualifiedActionName
//*                      / qualifiedFunctionName
//*                      / selectListProperty
//*                      )
named!(selectListItem<Input, Input, Error>, call!(alt((STAR
					, allOperationsInSchema
					, recognize(tuple((opt(tuple((qualifiedEntityTypeName, tag("/")))), alt((qualifiedActionName, qualifiedFunctionName, selectListProperty)))))
				       ))));
//* selectListProperty = primitiveProperty
//*                    / primitiveColProperty
//*                    / navigationProperty [ "+" ] [ selectList ]
//*                    / selectPath [ "/" selectListProperty ]
named!(selectListProperty<Input, Input, Error>, call!(alt((primitiveProperty
					    , primitiveColProperty
					    , recognize(tuple((navigationProperty, opt(tag("+")), opt(selectList))))
					    , recognize(tuple((selectPath, opt(tuple((tag("/"), selectListProperty))))))
					   ))));
//*
//* contextPropertyPath = primitiveProperty
//*                     / primitiveColProperty
//*                     / complexColProperty
//*                     / complexProperty [ [ "/" qualifiedComplexTypeName ] "/" contextPropertyPath ]
named!(contextPropertyPath<Input, Input, Error>, call!(alt((primitiveProperty
					     , primitiveColProperty
					     , complexColProperty
					     , recognize(tuple((complexProperty, opt(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), tag("/"), contextPropertyPath))))))
					     ))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 4. Expressions
//* ;------------------------------------------------------------------------------
//*
//* ; Note: a boolCommonExpr is also a commonExpr, e.g. sort by Boolean
//* commonExpr = ( primitiveLiteral
//*              / arrayOrObject
//*              / rootExpr
//*              / firstMemberExpr
//*              / functionExpr
//*              / negateExpr
//*              / methodCallExpr
//*              / parenExpr
//*              / listExpr
//*              / castExpr
//*              / isofExpr
//*              / notExpr
//*              )
//*              [ addExpr
//*              / subExpr
//*              / mulExpr
//*              / divExpr
//*              / divbyExpr
//*              / modExpr
//*              ]
//*              [ eqExpr
//*              / neExpr
//*              / ltExpr
//*              / leExpr
//*              / gtExpr
//*              / geExpr
//*              / hasExpr
//*              / inExpr
//*              ]
//*              [ andExpr
//*              / orExpr
//*              ]
named!(commonExpr<Input, Input, Error>, call!(recognize(tuple((
						 alt((
							 primitiveLiteral
							 , arrayOrObject
							 , rootExpr
							 , firstMemberExpr
							 , functionExpr
							 , negateExpr
							 , methodCallExpr
							 , parenExpr
							 , listExpr
							 , castExpr
							 , isofExpr
							 , notExpr
						 )),
						 opt(alt((
							 addExpr
							 , subExpr
							 , mulExpr
							 , divExpr
							 , divbyExpr
							 , modExpr
						 ))),
						 opt(alt((
							 eqExpr
							 , neExpr
							 , ltExpr
							 , leExpr
							 , gtExpr
							 , geExpr
							 , hasExpr
							 , inExpr
						 ))),
						 opt(alt((andExpr, orExpr)))
					)))));
fn commonExpr_wip<'a>(input: Input<'a>, prec: u8) -> ExprOutput<'a> {
    let (mut input, mut lhs) = alt((
        expr(map(|i| primitiveLiteral_wip(i), ExprKind::Lit)),
        // arrayOrObject,
        // rootExpr,
        // expr(map(|i| firstMemberExpr_wip(i), ExprKind::Member)),
        // functionExpr,
        |i| negateExpr_wip(i),
        |i| methodCallExpr_wip(i),
        |i| parenExpr_wip(i),
        // castExpr,
        // isofExpr,
        |i| notExpr_wip(i),
    ))(input)?;

    // Precedence climbing. Avoids the excessive recursion that a standard recursive decent would
    // have to do in order to correctly parse precedence
    while let (i, Some(op)) = opt(verify(binop_wip, |op| op.precedence() >= prec))(input.clone())? {
        let prec = op.precedence();

        let (i, rhs) = match op {
            // A list expression is only valid after the `in` operator but it's also ambiguous with the
            // parenExpr if the list contains only one element. So we try it first
            ast::BinOp::In => alt((|i| listExpr_wip(i), |i| commonExpr_wip(i, prec)))(i)?,
            _ => commonExpr_wip(i, prec)?,
        };

        input = i;
        lhs = Rc::new(input.parser.expr(ExprKind::Binary(op, lhs, rhs)));
    }

    Ok((input, lhs))
}

fn binop_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, ast::BinOp, Error> {
    // TODO sort them by frequency for better performance
    // FIXME use FromStr just like the method call
    delimited(
        RWS,
        alt((
            value(ast::BinOp::Add, tag_no_case("add")),
            value(ast::BinOp::Sub, tag_no_case("sub")),
            value(ast::BinOp::Mul, tag_no_case("mul")),
            value(ast::BinOp::Div, tag_no_case("div")),
            value(ast::BinOp::DivBy, tag_no_case("divby")),
            value(ast::BinOp::Mod, tag_no_case("mod")),
            value(ast::BinOp::Eq, tag_no_case("eq")),
            value(ast::BinOp::Ne, tag_no_case("ne")),
            value(ast::BinOp::Lt, tag_no_case("lt")),
            value(ast::BinOp::Le, tag_no_case("le")),
            value(ast::BinOp::Gt, tag_no_case("gt")),
            value(ast::BinOp::Ge, tag_no_case("ge")),
            value(ast::BinOp::Has, tag_no_case("has")),
            value(ast::BinOp::In, tag_no_case("in")),
            value(ast::BinOp::And, tag_no_case("and")),
            value(ast::BinOp::Or, tag_no_case("or")),
        )),
        RWS,
    )(input)
}
//*
//* boolCommonExpr = commonExpr ; resulting in a Boolean
//TODO(validate)
named!(boolCommonExpr<Input, Input, Error>, call!(recognize(commonExpr)));
//*
//* rootExpr = '$root/' ( entitySetName keyPredicate / singletonEntity ) [ singleNavigationExpr ]
named!(rootExpr<Input, Input, Error>, call!(recognize(tuple((tag("$root/"), alt((recognize(tuple((entitySetName, keyPredicate))), singletonEntity)), opt(singleNavigationExpr))))));
//*
//* firstMemberExpr = memberExpr
//*                 / inscopeVariableExpr [ "/" memberExpr ]
named!(firstMemberExpr<Input, Input, Error>, call!(alt((memberExpr, recognize(tuple((inscopeVariableExpr, opt(tuple((tag("/"), memberExpr))))))))));
fn firstMemberExpr_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, Vec<ast::PathSegment<'a>>, Error> {
    // let (input, node) = alt((
    // 	|i| memberExpr_wip(i, &state),
    // 	|i| memberExpr_wip(i, &state),
    // ))(input)?;

    unimplemented!();

    // map(alt((
    // 	|i| memberExpr_wip(i),
    // 	|i| {
    // 		let (i, var_expr) = inscopeVariableExpr_wip(i)?;
    // 		let (i, member) = opt(preceded(tag("/"), |i| memberExpr_wip(i)))(i)?;

    // 		let mut ret = vec![var_expr];
    // 		if let Some(mut member) = member {
    // 			ret.append(&mut member);
    // 		}
    // 		Ok((i, ret))
    // 	},
    // )), |mut v| { v.reverse(); v })(input)
}
//*
//* memberExpr = [ qualifiedEntityTypeName "/" ]
//*              ( propertyPathExpr
//*              / boundFunctionExpr
//*              / annotationExpr
//*              )
named!(memberExpr<Input, Input, Error>, call!(recognize(tuple((opt(tuple((qualifiedEntityTypeName, tag("/")))), alt((propertyPathExpr, boundFunctionExpr, annotationExpr)))))));
fn memberExpr_wip<'a, 'b>(input: Input<'a>) -> IResult<Input<'a>, (), Error> {
    unimplemented!();
}
//*
//* propertyPathExpr = ( entityColNavigationProperty [ collectionNavigationExpr ]
//*                    / entityNavigationProperty    [ singleNavigationExpr ]
//*                    / complexColProperty          [ complexColPathExpr ]
//*                    / complexProperty             [ complexPathExpr ]
//*                    / primitiveColProperty        [ collectionPathExpr ]
//*                    / primitiveProperty           [ primitivePathExpr ]
//*                    / streamProperty              [ primitivePathExpr ]
//*                    )
named!(propertyPathExpr<Input, Input, Error>, call!(recognize(alt((
						     tuple((entityColNavigationProperty, opt(collectionNavigationExpr)))
						     , tuple((entityNavigationProperty, opt(singleNavigationExpr)))
						     , tuple((complexColProperty, opt(complexColPathExpr)))
						     , tuple((complexProperty, opt(complexPathExpr)))
						     , tuple((primitiveColProperty, opt(collectionPathExpr)))
						     , tuple((primitiveProperty, opt(primitivePathExpr)))
						     , tuple((streamProperty, opt(primitivePathExpr)))
						)))));
//*
//* annotationExpr = annotation
//*                  [ collectionPathExpr
//*                  / singleNavigationExpr
//*                  / complexPathExpr
//*                  / primitivePathExpr
//*                  ]
named!(annotationExpr<Input, Input, Error>, call!(recognize(tuple((annotation, opt(alt((collectionPathExpr, singleNavigationExpr, complexPathExpr, primitivePathExpr))))))));
//*
//* annotation          = AT [ namespace "." ] termName [ '#' annotationQualifier ]
named!(annotation<Input, Input, Error>, call!(recognize(tuple((AT, opt(tuple((namespace, tag(".")))), termName, opt(tuple((tag("#"), annotationQualifier))))))));
//* annotationQualifier = odataIdentifier
named!(annotationQualifier<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* inscopeVariableExpr  = implicitVariableExpr
//*                      / parameterAlias
//*                      / lambdaVariableExpr ; only allowed inside a lambdaPredicateExpr
//TODO(validation)
named!(inscopeVariableExpr<Input, Input, Error>, call!(alt((implicitVariableExpr, parameterAlias, lambdaVariableExpr))));
fn inscopeVariableExpr_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, ast::PathSegment<'a>, Error> {
    unimplemented!();
}
//* implicitVariableExpr = '$it'              ; the current instance of the resource identified by the resource path
//*                      / '$this'            ; the instance on which the query option is evaluated
named!(implicitVariableExpr<Input, Input, Error>, call!(alt((tag("$it"), tag("$this")))));
//* lambdaVariableExpr   = odataIdentifier
named!(lambdaVariableExpr<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* collectionNavigationExpr = [ "/" qualifiedEntityTypeName ]
//*                            [ keyPredicate [ singleNavigationExpr ]
//*                            / collectionPathExpr
//*                            ]
named!(collectionNavigationExpr<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(alt((
															   recognize(tuple((keyPredicate, opt(singleNavigationExpr))))
															   , collectionPathExpr
															))))))));
//*
//* singleNavigationExpr = "/" memberExpr
named!(singleNavigationExpr<Input, Input, Error>, call!(recognize(tuple((tag("/"), memberExpr)))));
//*
//* complexColPathExpr = [ "/" qualifiedComplexTypeName ]
//*                      [ collectionPathExpr ]
named!(complexColPathExpr<Input, Input, Error>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(collectionPathExpr))))));
//*
//* collectionPathExpr = count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                    / "/" boundFunctionExpr
//*                    / "/" annotationExpr
//*                    / "/" anyExpr
//*                    / "/" allExpr
named!(collectionPathExpr<Input, Input, Error>, call!(alt((
					    recognize(tuple((count, opt(tuple((OPEN, expandCountOption, many0(tuple((SEMI, expandCountOption))), CLOSE))))))
					    , recognize(tuple((tag("/"), boundFunctionExpr)))
					    , recognize(tuple((tag("/"), annotationExpr)))
					    , recognize(tuple((tag("/"), anyExpr)))
					    , recognize(tuple((tag("/"), allExpr)))
					   ))));
//*
//* complexPathExpr = [ "/" qualifiedComplexTypeName ]
//*                   [ "/" propertyPathExpr
//*                   / "/" boundFunctionExpr
//*                   / "/" annotationExpr
//*                   ]
named!(complexPathExpr<Input, Input, Error>, call!(recognize(tuple((
						      opt(tuple((tag("/"), qualifiedComplexTypeName))),
						      opt(alt((
								tuple((tag("/"), propertyPathExpr))
								, tuple((tag("/"), boundFunctionExpr))
								, tuple((tag("/"), annotationExpr))
						      )))
						)))));
//*
//* primitivePathExpr = "/" [ annotationExpr / boundFunctionExpr ]
named!(primitivePathExpr<Input, Input, Error>, call!(recognize(tuple((tag("/"), opt(alt((annotationExpr, boundFunctionExpr))))))));
//*
//* boundFunctionExpr = functionExpr ; boundFunction segments can only be composed if the type of the
//*                                  ; previous segment matches the type of the first function parameter
//TODO(validation)
named!(boundFunctionExpr<Input, Input, Error>, call!(recognize(functionExpr)));
//*
//* functionExpr = namespace "."
//*                ( entityColFunction    functionExprParameters [ collectionNavigationExpr ]
//*                / entityFunction       functionExprParameters [ singleNavigationExpr ]
//*                / complexColFunction   functionExprParameters [ complexColPathExpr ]
//*                / complexFunction      functionExprParameters [ complexPathExpr ]
//*                / primitiveColFunction functionExprParameters [ collectionPathExpr ]
//*                / primitiveFunction    functionExprParameters [ primitivePathExpr ]
//*                )
named!(functionExpr<Input, Input, Error>, call!(recognize(tuple((namespace,
						   tag("."),
						   alt((
							recognize(tuple((entityColFunction, functionExprParameters, opt(collectionNavigationExpr))))
							, recognize(tuple((entityFunction, functionExprParameters, opt(singleNavigationExpr))))
							, recognize(tuple((complexColFunction, functionExprParameters, opt(complexColPathExpr))))
							, recognize(tuple((complexFunction, functionExprParameters, opt(complexPathExpr))))
							, recognize(tuple((primitiveColFunction, functionExprParameters, opt(collectionPathExpr))))
							, recognize(tuple((primitiveFunction, functionExprParameters, opt(primitivePathExpr))))
						   )))))));
//*
//* functionExprParameters = OPEN [ functionExprParameter *( COMMA functionExprParameter ) ] CLOSE
named!(functionExprParameters<Input, Input, Error>, call!(recognize(tuple((OPEN, opt(tuple((functionExprParameter, many0(tuple((COMMA, functionExprParameter)))))), CLOSE)))));
//* functionExprParameter  = parameterName EQ ( parameterAlias / parameterValue )
named!(functionExprParameter<Input, Input, Error>, call!(recognize(tuple((parameterName, EQ, alt((parameterAlias, parameterValue)))))));
//*
//* anyExpr = "any" OPEN BWS [ lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr ] BWS CLOSE
named!(anyExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("any"), OPEN, BWS, opt(tuple((lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr))), BWS, CLOSE)))));
//* allExpr = "all" OPEN BWS   lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr   BWS CLOSE
named!(allExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("all"), OPEN, BWS, lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr, BWS, CLOSE)))));
//* lambdaPredicateExpr = boolCommonExpr ; containing at least one lambdaVariableExpr
//TODO(use verify() to verify that it contains at least one lambdaVariableExpr)
named!(lambdaPredicateExpr<Input, Input, Error>, call!(recognize(boolCommonExpr)));
//*
//* methodCallExpr = indexOfMethodCallExpr
//*                / toLowerMethodCallExpr
//*                / toUpperMethodCallExpr
//*                / trimMethodCallExpr
//*                / substringMethodCallExpr
//*                / concatMethodCallExpr
//*                / lengthMethodCallExpr
//*                / yearMethodCallExpr
//*                / monthMethodCallExpr
//*                / dayMethodCallExpr
//*                / hourMethodCallExpr
//*                / minuteMethodCallExpr
//*                / secondMethodCallExpr
//*                / fractionalsecondsMethodCallExpr
//*                / totalsecondsMethodCallExpr
//*                / dateMethodCallExpr
//*                / timeMethodCallExpr
//*                / roundMethodCallExpr
//*                / floorMethodCallExpr
//*                / ceilingMethodCallExpr
//*                / distanceMethodCallExpr
//*                / geoLengthMethodCallExpr
//*                / totalOffsetMinutesMethodCallExpr
//*                / minDateTimeMethodCallExpr
//*                / maxDateTimeMethodCallExpr
//*                / nowMethodCallExpr
//*                / boolMethodCallExpr
named!(methodCallExpr<Input, Input, Error>, call!(alt((
						alt((indexOfMethodCallExpr
							, toLowerMethodCallExpr
							, toUpperMethodCallExpr
							, trimMethodCallExpr
							, substringMethodCallExpr
							, concatMethodCallExpr
							, lengthMethodCallExpr
							, yearMethodCallExpr
							, monthMethodCallExpr
							, dayMethodCallExpr
							, hourMethodCallExpr
							, minuteMethodCallExpr
							, secondMethodCallExpr
							, fractionalsecondsMethodCallExpr
						)),
						alt((
							totalsecondsMethodCallExpr
							, dateMethodCallExpr
							, timeMethodCallExpr
							, roundMethodCallExpr
							, floorMethodCallExpr
							, ceilingMethodCallExpr
							, distanceMethodCallExpr
							, geoLengthMethodCallExpr
							, totalOffsetMinutesMethodCallExpr
							, minDateTimeMethodCallExpr
							, maxDateTimeMethodCallExpr
							, nowMethodCallExpr
							, boolMethodCallExpr
						))
					))));
fn methodCallExpr_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    let (input, method) = map_res(take_while1(|c: char| c.is_alphabetic()), |res: Input| {
        ast::Method::from_str(res.data)
    })(input)?;

    let (m, n) = method.arity();

    let arg_parser = verify(
        separated_list(tuple((BWS, COMMA, BWS)), |i| commonExpr_wip(i, 0)),
        move |v: &Vec<Rc<ast::Expr<'a>>>| m <= v.len() && v.len() <= n,
    );

    expr(map(
        delimited(tuple((OPEN, BWS)), arg_parser, tuple((BWS, CLOSE))),
        move |args| ExprKind::MethodCall(method, args),
    ))(input)
}
//*
//* boolMethodCallExpr = endsWithMethodCallExpr
//*                    / startsWithMethodCallExpr
//*                    / containsMethodCallExpr
//*                    / intersectsMethodCallExpr
//*                    / hasSubsetMethodCallExpr
//*                    / hasSubsequenceMethodCallExpr
named!(boolMethodCallExpr<Input, Input, Error>, call!(alt((endsWithMethodCallExpr
					    , startsWithMethodCallExpr
					    , containsMethodCallExpr
					    , intersectsMethodCallExpr
					    , hasSubsetMethodCallExpr
					    , hasSubsequenceMethodCallExpr))));
//*
//* concatMethodCallExpr     = "concat"     OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(concatMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("concat"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* containsMethodCallExpr   = "contains"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(containsMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("contains"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* endsWithMethodCallExpr   = "endswith"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(endsWithMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("endswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* indexOfMethodCallExpr    = "indexof"    OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(indexOfMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("indexof"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* lengthMethodCallExpr     = "length"     OPEN BWS commonExpr BWS CLOSE
named!(lengthMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("length"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* startsWithMethodCallExpr = "startswith" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(startsWithMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("startswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* substringMethodCallExpr  = "substring"  OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS [ COMMA BWS commonExpr BWS ] CLOSE
named!(substringMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("substring"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, opt(tuple((COMMA, BWS, commonExpr, BWS))), CLOSE)))));
//* toLowerMethodCallExpr    = "tolower"    OPEN BWS commonExpr BWS CLOSE
named!(toLowerMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("tolower"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* toUpperMethodCallExpr    = "toupper"    OPEN BWS commonExpr BWS CLOSE
named!(toUpperMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("toupper"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* trimMethodCallExpr       = "trim"       OPEN BWS commonExpr BWS CLOSE
named!(trimMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("trim"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* yearMethodCallExpr               = "year"               OPEN BWS commonExpr BWS CLOSE
named!(yearMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("year"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* monthMethodCallExpr              = "month"              OPEN BWS commonExpr BWS CLOSE
named!(monthMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("month"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* dayMethodCallExpr                = "day"                OPEN BWS commonExpr BWS CLOSE
named!(dayMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("day"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* hourMethodCallExpr               = "hour"               OPEN BWS commonExpr BWS CLOSE
named!(hourMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("hour"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* minuteMethodCallExpr             = "minute"             OPEN BWS commonExpr BWS CLOSE
named!(minuteMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("minute"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* secondMethodCallExpr             = "second"             OPEN BWS commonExpr BWS CLOSE
named!(secondMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("second"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* fractionalsecondsMethodCallExpr  = "fractionalseconds"  OPEN BWS commonExpr BWS CLOSE
named!(fractionalsecondsMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("fractionalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* totalsecondsMethodCallExpr       = "totalseconds"       OPEN BWS commonExpr BWS CLOSE
named!(totalsecondsMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("totalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* dateMethodCallExpr               = "date"               OPEN BWS commonExpr BWS CLOSE
named!(dateMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("date"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* timeMethodCallExpr               = "time"               OPEN BWS commonExpr BWS CLOSE
named!(timeMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("time"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* totalOffsetMinutesMethodCallExpr = "totaloffsetminutes" OPEN BWS commonExpr BWS CLOSE
named!(totalOffsetMinutesMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("totaloffsetminutes"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* minDateTimeMethodCallExpr = "mindatetime" OPEN BWS CLOSE
named!(minDateTimeMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("mindatetime"), OPEN, BWS, CLOSE)))));
//* maxDateTimeMethodCallExpr = "maxdatetime" OPEN BWS CLOSE
named!(maxDateTimeMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("maxdatetime"), OPEN, BWS, CLOSE)))));
//* nowMethodCallExpr         = "now"         OPEN BWS CLOSE
named!(nowMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("now"), OPEN, BWS, CLOSE)))));
//*
//* roundMethodCallExpr   = "round"   OPEN BWS commonExpr BWS CLOSE
named!(roundMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("round"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* floorMethodCallExpr   = "floor"   OPEN BWS commonExpr BWS CLOSE
named!(floorMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("floor"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* ceilingMethodCallExpr = "ceiling" OPEN BWS commonExpr BWS CLOSE
named!(ceilingMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("ceiling"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* distanceMethodCallExpr   = "geo.distance"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(distanceMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("geo.distance"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* geoLengthMethodCallExpr  = "geo.length"     OPEN BWS commonExpr BWS CLOSE
named!(geoLengthMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("geo.length"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* intersectsMethodCallExpr = "geo.intersects" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(intersectsMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("geo.intersects"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//*
//* hasSubsetMethodCallExpr      = "hassubset"      OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsetMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("hassubset"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* hasSubsequenceMethodCallExpr = "hassubsequence" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsequenceMethodCallExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("hassubsequence"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//*
//* parenExpr = OPEN BWS commonExpr BWS CLOSE
named!(parenExpr<Input, Input, Error>, call!(recognize(tuple((OPEN, BWS, commonExpr, BWS, CLOSE)))));
fn parenExpr_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    delimited(
        tuple((OPEN, BWS)),
        |i| commonExpr_wip(i, 0),
        tuple((BWS, CLOSE)),
    )(input)
}
//* listExpr  = OPEN BWS commonExpr BWS *( COMMA BWS commonExpr BWS ) CLOSE
named!(listExpr<Input, Input, Error>, call!(recognize(tuple((OPEN, BWS, commonExpr, many0(tuple((COMMA, BWS, commonExpr, BWS))), CLOSE)))));
fn listExpr_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    let element_parser = expr(map(
        separated_nonempty_list(tuple((BWS, COMMA, BWS)), |i| commonExpr_wip(i, 0)),
        ExprKind::List,
    ));
    delimited(tuple((OPEN, BWS)), element_parser, tuple((BWS, CLOSE)))(input)
}
//*
//* andExpr = RWS "and" RWS boolCommonExpr
named!(andExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("and"), RWS, boolCommonExpr)))));
//* orExpr  = RWS "or"  RWS boolCommonExpr
named!(orExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("or"), RWS, boolCommonExpr)))));
//*
//* eqExpr = RWS "eq" RWS commonExpr
named!(eqExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("eq"), RWS, commonExpr)))));
//* neExpr = RWS "ne" RWS commonExpr
named!(neExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("ne"), RWS, commonExpr)))));
//* ltExpr = RWS "lt" RWS commonExpr
named!(ltExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("lt"), RWS, commonExpr)))));
//* leExpr = RWS "le" RWS commonExpr
named!(leExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("le"), RWS, commonExpr)))));
//* gtExpr = RWS "gt" RWS commonExpr
named!(gtExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("gt"), RWS, commonExpr)))));
//* geExpr = RWS "ge" RWS commonExpr
named!(geExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("ge"), RWS, commonExpr)))));
//* inExpr = RWS "in" RWS commonExpr
named!(inExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("in"), RWS, commonExpr)))));
//*
//* hasExpr = RWS "has" RWS enum
named!(hasExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("has"), RWS, commonExpr)))));
//*
//* addExpr   = RWS "add"   RWS commonExpr
named!(addExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("add"), RWS, commonExpr)))));
//* subExpr   = RWS "sub"   RWS commonExpr
named!(subExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("sub"), RWS, commonExpr)))));
//* mulExpr   = RWS "mul"   RWS commonExpr
named!(mulExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("mul"), RWS, commonExpr)))));
//* divExpr   = RWS "div"   RWS commonExpr
named!(divExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("div"), RWS, commonExpr)))));
//* divbyExpr = RWS "divby" RWS commonExpr
named!(divbyExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("divby"), RWS, commonExpr)))));
//* modExpr   = RWS "mod"   RWS commonExpr
named!(modExpr<Input, Input, Error>, call!(recognize(tuple((RWS, tag_no_case("mod"), RWS, commonExpr)))));
//*
//* negateExpr = "-" BWS commonExpr
named!(negateExpr<Input, Input, Error>, call!(recognize(tuple((tag("-"), BWS, commonExpr)))));
fn negateExpr_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    let op = ast::UnOp::Neg;
    expr(map(
        preceded(tag("-"), move |i| commonExpr_wip(i, op.precedence())),
        move |expr| ExprKind::Unary(op, expr),
    ))(input)
}
//*
//* notExpr = "not" RWS boolCommonExpr
named!(notExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("not"), RWS, boolCommonExpr)))));
fn notExpr_wip<'a>(input: Input<'a>) -> ExprOutput<'a> {
    let op = ast::UnOp::Not;
    expr(map(
        preceded(tag("not"), move |i| commonExpr_wip(i, op.precedence())),
        move |expr| ExprKind::Unary(op, expr),
    ))(input)
}
//*
//* isofExpr = "isof" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(isofExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("isof"), OPEN, BWS, opt(tuple((commonExpr, BWS, COMMA, BWS))), qualifiedTypeName, BWS, CLOSE)))));
//* castExpr = "cast" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(castExpr<Input, Input, Error>, call!(recognize(tuple((tag_no_case("cast"), OPEN, BWS, opt(tuple((commonExpr, BWS, COMMA, BWS))), qualifiedTypeName, BWS, CLOSE)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 5. JSON format for function parameters
//* ;------------------------------------------------------------------------------
//* ; Note: the query part of a URI needs to be partially percent-decoded before
//* ; applying these rules, see comment at the top of this file
//* ;------------------------------------------------------------------------------
//*
//* arrayOrObject = complexColInUri
//*               / complexInUri
//*               / rootExprCol
//*               / primitiveColInUri
named!(arrayOrObject<Input, Input, Error>, call!(alt((complexColInUri, complexInUri, rootExprCol, primitiveColInUri))));
//*
//* complexColInUri = begin-array
//*                   [ complexInUri *( value-separator complexInUri ) ]
//*                   end-array
named!(complexColInUri<Input, Input, Error>, call!(recognize(tuple((begin_array, opt(tuple((complexInUri, many0(tuple((value_separator, complexInUri)))))), end_array)))));
//*
//* complexInUri = begin-object
//*                [ ( annotationInUri
//*                  / primitivePropertyInUri
//*                  / complexPropertyInUri
//*                  / collectionPropertyInUri
//*                  / navigationPropertyInUri
//*                  )
//*                  *( value-separator
//*                     ( annotationInUri
//*                     / primitivePropertyInUri
//*                     / complexPropertyInUri
//*                     / collectionPropertyInUri
//*                     / navigationPropertyInUri
//*                     )
//*                   )
//*                ]
//*                end-object
named!(complexInUri<Input, Input, Error>, call!(recognize(tuple((begin_object,
						   opt(tuple((
							       alt((  annotationInUri
								    , primitivePropertyInUri
								    , complexPropertyInUri
								    , collectionPropertyInUri
								    , navigationPropertyInUri)),
								many0(tuple((
									      value_separator,
									      alt((  annotationInUri
										   , primitivePropertyInUri
										   , complexPropertyInUri
										   , collectionPropertyInUri
										   , navigationPropertyInUri))
								)))
						    ))),
						    end_object
						)))));

//*
//* collectionPropertyInUri = ( quotation-mark primitiveColProperty quotation-mark
//*                             name-separator
//*                             primitiveColInUri
//*                           )
//*                         / ( quotation-mark complexColProperty quotation-mark
//*                             name-separator
//*                             complexColInUri
//*                           )
named!(collectionPropertyInUri<Input, Input, Error>, call!(alt((recognize(tuple((quotation_mark, primitiveColProperty, quotation_mark, name_separator, primitiveColInUri)))
						 , recognize(tuple((quotation_mark, complexColProperty, quotation_mark, name_separator, complexColInUri)))))));

//*
//* primitiveColInUri = begin-array
//*                     [ primitiveLiteralInJSON *( value-separator primitiveLiteralInJSON ) ]
//*                     end-array
named!(primitiveColInUri<Input, Input, Error>, call!(recognize(tuple((begin_array, opt(tuple((primitiveLiteralInJSON, many0(tuple((value_separator, primitiveLiteralInJSON)))))), end_array)))));
//*
//* complexPropertyInUri = quotation-mark complexProperty quotation-mark
//*                        name-separator
//*                        complexInUri
named!(complexPropertyInUri<Input, Input, Error>, call!(recognize(tuple((quotation_mark, complexProperty, quotation_mark, name_separator, complexInUri)))));
//*
//* annotationInUri = quotation-mark AT namespace "." termName quotation-mark
//*                   name-separator
//*                   ( complexInUri / complexColInUri / primitiveLiteralInJSON / primitiveColInUri )
named!(annotationInUri<Input, Input, Error>, call!(recognize(tuple((quotation_mark, AT, namespace, tag("."), termName, quotation_mark,
						      name_separator,
						      alt((complexInUri, complexColInUri, primitiveLiteralInJSON, primitiveColInUri)))))));
//*
//* primitivePropertyInUri = quotation-mark primitiveProperty quotation-mark
//*                          name-separator
//*                          primitiveLiteralInJSON
named!(primitivePropertyInUri<Input, Input, Error>, call!(recognize(tuple((quotation_mark, primitiveProperty, quotation_mark, name_separator, primitiveLiteralInJSON)))));
//*
//* navigationPropertyInUri = singleNavPropInJSON
//*                         / collectionNavPropInJSON
named!(navigationPropertyInUri<Input, Input, Error>, call!(alt((singleNavPropInJSON, collectionNavPropInJSON))));
//* singleNavPropInJSON     = quotation-mark entityNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExpr
named!(singleNavPropInJSON<Input, Input, Error>, call!(recognize(tuple((quotation_mark, entityNavigationProperty, quotation_mark, name_separator, rootExpr)))));
//* collectionNavPropInJSON = quotation-mark entityColNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExprCol
named!(collectionNavPropInJSON<Input, Input, Error>, call!(recognize(tuple((quotation_mark, entityColNavigationProperty, quotation_mark, name_separator, rootExprCol)))));
//*
//* rootExprCol = begin-array
//*               [ rootExpr *( value-separator rootExpr ) ]
//*               end-array
named!(rootExprCol<Input, Input, Error>, call!(recognize(tuple((begin_array, opt(tuple((rootExpr, many0(tuple((value_separator, rootExpr)))))), end_array)))));
//*
//* ; JSON syntax: adapted to URI restrictions from [RFC4627]
//* begin-object = BWS ( "{" / "%7B" ) BWS
named!(begin_object<Input, Input, Error>, call!(recognize(tuple((BWS, alt((tag("{"), tag("%7B"))))))));
//* end-object   = BWS ( "}" / "%7D" )
named!(end_object<Input, Input, Error>, call!(recognize(tuple((BWS, alt((tag("}"), tag("%7D"))))))));
//*
//* begin-array = BWS ( "[" / "%5B" ) BWS
named!(begin_array<Input, Input, Error>, call!(recognize(tuple((BWS, alt((tag("["), tag("%5B"))))))));
//* end-array   = BWS ( "]" / "%5D" )
named!(end_array<Input, Input, Error>, call!(recognize(tuple((BWS, alt((tag("]"), tag("%5D"))))))));
//*
//* quotation-mark  = DQUOTE / "%22"
named!(quotation_mark<Input, Input, Error>, call!(alt((recognize(DQUOTE), tag("%22")))));
//* name-separator  = BWS COLON BWS
named!(name_separator<Input, Input, Error>, call!(recognize(tuple((BWS, COLON, BWS)))));
//* value-separator = BWS COMMA BWS
named!(value_separator<Input, Input, Error>, call!(recognize(tuple((BWS, COMMA, BWS)))));
//*
//* primitiveLiteralInJSON = stringInJSON
//*                        / numberInJSON
//*                        / 'true'
//*                        / 'false'
//*                        / 'null'
named!(primitiveLiteralInJSON<Input, Input, Error>, call!(alt((stringInJSON, numberInJSON, tag("true"), tag("false"), tag("null")))));
//*
//* stringInJSON = quotation-mark *charInJSON quotation-mark
named!(stringInJSON<Input, Input, Error>, call!(recognize(tuple((quotation_mark, many0(charInJSON), quotation_mark)))));
//* charInJSON   = qchar-unescaped
//*              / qchar-JSON-special
//*              / escape ( quotation-mark
//*                       / escape
//*                       / ( "/" / "%2F" ) ; solidus         U+002F - literal form is allowed in the query part of a URL
//*                       / 'b'             ; backspace       U+0008
//*                       / 'f'             ; form feed       U+000C
//*                       / 'n'             ; line feed       U+000A
//*                       / 'r'             ; carriage return U+000D
//*                       / 't'             ; tab             U+0009
//*                       / 'u' 4HEXDIG     ;                 U+XXXX
//*                       )
named!(charInJSON<Input, Input, Error>, call!(alt((qchar_unescaped, qchar_JSON_special, recognize(tuple((escape, alt((  quotation_mark
													  , escape
													  , alt((tag("/"), tag("%2F")))
													  , recognize(one_of("bfnrt"))
													  , recognize(tuple((tag("u"), many_m_n(4, 4, HEXDIG))))
													  )))))))));
//*
//* qchar-JSON-special = SP / ":" / "{" / "}" / "[" / "]" ; some agents put these unencoded into the query part of a URL
named!(qchar_JSON_special<Input, Input, Error>, call!(alt((SP, recognize(one_of(":{}[]"))))));
//*
//* escape = "\" / "%5C"     ; reverse solidus U+005C
named!(escape<Input, Input, Error>, call!(alt((tag("\\"), tag("%5C")))));
//*
//* numberInJSON = [ "-" ] int [ frac ] [ exp ]
named!(numberInJSON<Input, Input, Error>, call!(recognize(tuple((opt(tag("-")), int, opt(frac), opt(exp))))));
//* int          = "0" / ( oneToNine *DIGIT )
named!(int<Input, Input, Error>, call!(alt((tag("0"), recognize(tuple((oneToNine, many0(DIGIT))))))));
//* frac         = "." 1*DIGIT
named!(frac<Input, Input, Error>, call!(recognize(tuple((tag("."), many1(DIGIT))))));
//* exp          = "e" [ "-" / "+" ] 1*DIGIT
named!(exp<Input, Input, Error>, call!(recognize(tuple((tag_no_case("e"), opt(alt((tag("-"), tag("+")))), many1(DIGIT))))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 6. Names and identifiers
//* ;------------------------------------------------------------------------------
//*
//* singleQualifiedTypeName = qualifiedEntityTypeName
//*                         / qualifiedComplexTypeName
//*                         / qualifiedTypeDefinitionName
//*                         / qualifiedEnumTypeName
//*                         / primitiveTypeName
named!(singleQualifiedTypeName<Input, Input, Error>, call!(alt((qualifiedEntityTypeName
						 , qualifiedComplexTypeName
						 , qualifiedTypeDefinitionName
						 , qualifiedEnumTypeName
						 , primitiveTypeName))));
//*
//* qualifiedTypeName = singleQualifiedTypeName
//*                   / 'Collection' OPEN singleQualifiedTypeName CLOSE
named!(qualifiedTypeName<Input, Input, Error>, call!(alt((singleQualifiedTypeName
					   , recognize(tuple((tag("Collection"), OPEN, singleQualifiedTypeName, CLOSE)))))));
//*
//* qualifiedEntityTypeName     = namespace "." entityTypeName
named!(qualifiedEntityTypeName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), entityTypeName)))));
fn qualifiedEntityTypeName_wip<'a>(input: Input<'a>, child: &Rc<Expr<'a>>) -> ExprOutput<'a> {
    let (input, (namespace, entity)) = namespaced_item(input)?;

    input.parser.document.schemas
        .get(namespace)
        .and_then(|s| s.entity_types.get(entity))
        .map(|e| (input.clone(), Rc::new(input.parser.expr(ExprKind::Cast(&e, Rc::clone(child))))))
        .ok_or(Err::Error(()))
}

//* qualifiedComplexTypeName    = namespace "." complexTypeName
named!(qualifiedComplexTypeName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), complexTypeName)))));
//* qualifiedTypeDefinitionName = namespace "." typeDefinitionName
named!(qualifiedTypeDefinitionName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), typeDefinitionName)))));
//* qualifiedEnumTypeName       = namespace "." enumerationTypeName
named!(qualifiedEnumTypeName<Input, Input, Error>, call!(recognize(tuple((namespace, tag("."), enumerationTypeName)))));

/// Utility function to help with namespace rules that would normally require lookahead to do
/// properly
fn namespaced_item<'a>(input: Input<'a>) -> IResult<Input<'a>, (&'a str, &'a str), Error> {
    let input2 = input.clone();

    let (mut input, (_, mut sep,  mut item)) = tuple((odataIdentifier, tag("."), odataIdentifier))(input)?;

    while let Ok((i, (s, it))) = tuple((tag("."), odataIdentifier))(input.clone()) {
        input = i;
        sep = s;
        item = it;
    }

    let namespace = input2.slice(..input2.offset(&sep));

    Ok((input, (namespace.data, item.data)))
}
//*
//* ; an alias is just a single-part namespace
//* namespace     = namespacePart *( "." namespacePart )
named!(namespace<Input, Input, Error>, call!(recognize(tuple((namespacePart, many0(tuple((tag("."), namespacePart))))))));
//* namespacePart = odataIdentifier
named!(namespacePart<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* entitySetName       = odataIdentifier
fn entitySetName_wip<'a>(
    input: Input<'a>,
    container: &'a schema::EntityContainer<'a>,
) -> IResult<Input<'a>, &'a schema::EntitySet<'a>, Error> {
    map_opt(recognize(odataIdentifier), |name| {
        container.entity_sets.get(name.data)
    })(input)
}
named!(entitySetName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* singletonEntity     = odataIdentifier
named!(singletonEntity<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* entityTypeName      = odataIdentifier
named!(entityTypeName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexTypeName     = odataIdentifier
named!(complexTypeName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* typeDefinitionName  = odataIdentifier
named!(typeDefinitionName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* enumerationTypeName = odataIdentifier
named!(enumerationTypeName<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* enumerationMember   = odataIdentifier
named!(enumerationMember<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* termName            = odataIdentifier
named!(termName<Input, Input, Error>, call!(recognize(odataIdentifier)));

//TODO(restrictive + unicode)
//* ; Note: this pattern is overly restrictive, the normative definition is type TSimpleIdentifier in OData EDM XML Schema
//* odataIdentifier             = identifierLeadingCharacter *127identifierCharacter
named!(odataIdentifier<Input, Input, Error>, call!(recognize(tuple((identifierLeadingCharacter, many_m_n(0, 127, identifierCharacter))))));
//* identifierLeadingCharacter  = ALPHA / "_"         ; plus Unicode characters from the categories L or Nl
named!(identifierLeadingCharacter<Input, Input, Error>, call!(alt((ALPHA, tag("_")))));
//* identifierCharacter         = ALPHA / "_" / DIGIT ; plus Unicode characters from the categories L, Nl, Nd, Mn, Mc, Pc, or Cf
named!(identifierCharacter<Input, Input, Error>, call!(alt((ALPHA, tag("_"), DIGIT))));
//*
//* primitiveTypeName = 'Edm.' ( 'Binary'
//*                            / 'Boolean'
//*                            / 'Byte'
//*                            / 'Date'
//*                            / 'DateTimeOffset'
//*                            / 'Decimal'
//*                            / 'Double'
//*                            / 'Duration'
//*                            / 'Guid'
//*                            / 'Int16'
//*                            / 'Int32'
//*                            / 'Int64'
//*                            / 'SByte'
//*                            / 'Single'
//*                            / 'Stream'
//*                            / 'String'
//*                            / 'TimeOfDay'
//*                            / abstractSpatialTypeName [ concreteSpatialTypeName ]
//*                            )
named!(primitiveTypeName<Input, Input, Error>, call!(recognize(tuple((tag("Edm."), alt((  tag("Binary")
									   , tag("Boolean")
									   , tag("Byte")
									   , tag("Date")
									   , tag("DateTimeOffset")
									   , tag("Decimal")
									   , tag("Double")
									   , tag("Duration")
									   , tag("Guid")
									   , tag("Int16")
									   , tag("Int32")
									   , tag("Int64")
									   , tag("SByte")
									   , tag("Single")
									   , tag("Stream")
									   , tag("String")
									   , tag("TimeOfDay")
									   , recognize(tuple((abstractSpatialTypeName, opt(concreteSpatialTypeName))))
									)))))));
//* abstractSpatialTypeName = 'Geography'
//*                         / 'Geometry'
named!(abstractSpatialTypeName<Input, Input, Error>, call!(alt((tag("Geography"), tag("Geometry")))));
//* concreteSpatialTypeName = 'Collection'
//*                         / 'LineString'
//*                         / 'MultiLineString'
//*                         / 'MultiPoint'
//*                         / 'MultiPolygon'
//*                         / 'Point'
//*                         / 'Polygon'
named!(concreteSpatialTypeName<Input, Input, Error>, call!(alt((  tag("Collection")
						 , tag("LineString")
						 , tag("MultiLineString")
						 , tag("MultiPoint")
						 , tag("MultiPolygon")
						 , tag("Point")
						 , tag("Polygon")
						 ))));
//*
//* primitiveProperty       = primitiveKeyProperty / primitiveNonKeyProperty
named!(primitiveProperty<Input, Input, Error>, call!(alt((primitiveKeyProperty, primitiveNonKeyProperty))));
//* primitiveKeyProperty    = odataIdentifier
named!(primitiveKeyProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveNonKeyProperty = odataIdentifier
named!(primitiveNonKeyProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveColProperty    = odataIdentifier
named!(primitiveColProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexProperty         = odataIdentifier
named!(complexProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexColProperty      = odataIdentifier
named!(complexColProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* streamProperty          = odataIdentifier
named!(streamProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* navigationProperty          = entityNavigationProperty / entityColNavigationProperty
named!(navigationProperty<Input, Input, Error>, call!(alt((entityNavigationProperty, entityColNavigationProperty))));
//* entityNavigationProperty    = odataIdentifier
named!(entityNavigationProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* entityColNavigationProperty = odataIdentifier
named!(entityColNavigationProperty<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* action       = odataIdentifier
named!(action<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* actionImport = odataIdentifier
named!(actionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* function = entityFunction
//*          / entityColFunction
//*          / complexFunction
//*          / complexColFunction
//*          / primitiveFunction
//*          / primitiveColFunction
named!(function<Input, Input, Error>, call!(alt(( entityFunction
				 , entityColFunction
				 , complexFunction
				 , complexColFunction
				 , primitiveFunction
				 , primitiveColFunction))));
//*
//* entityFunction       = odataIdentifier
named!(entityFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* entityColFunction    = odataIdentifier
named!(entityColFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexFunction      = odataIdentifier
named!(complexFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexColFunction   = odataIdentifier
named!(complexColFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveFunction    = odataIdentifier
named!(primitiveFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveColFunction = odataIdentifier
named!(primitiveColFunction<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//* entityFunctionImport       = odataIdentifier
named!(entityFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* entityColFunctionImport    = odataIdentifier
named!(entityColFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexFunctionImport      = odataIdentifier
named!(complexFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* complexColFunctionImport   = odataIdentifier
named!(complexColFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveFunctionImport    = odataIdentifier
named!(primitiveFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//* primitiveColFunctionImport = odataIdentifier
named!(primitiveColFunctionImport<Input, Input, Error>, call!(recognize(odataIdentifier)));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 7. Literal Data Values
//* ;------------------------------------------------------------------------------
//*
//* ; in URLs
//* primitiveLiteral = nullValue                  ; plain values up to int64Value
//*                  / booleanValue
//*                  / guidValue
//*                  / dateValue
//*                  / dateTimeOffsetValue
//*                  / timeOfDayValue
//*                  / decimalValue
//*                  / doubleValue
//*                  / singleValue
//*                  / sbyteValue
//*                  / byteValue
//*                  / int16Value
//*                  / int32Value
//*                  / int64Value
//*                  / string                     ; single-quoted
//*                  / duration
//*                  / enum
//*                  / binary                     ; all others are quoted and prefixed
//*                  / geographyCollection
//*                  / geographyLineString
//*                  / geographyMultiLineString
//*                  / geographyMultiPoint
//*                  / geographyMultiPolygon
//*                  / geographyPoint
//*                  / geographyPolygon
//*                  / geometryCollection
//*                  / geometryLineString
//*                  / geometryMultiLineString
//*                  / geometryMultiPoint
//*                  / geometryMultiPolygon
//*                  / geometryPoint
//*                  / geometryPolygon
named!(primitiveLiteral<Input, Input, Error>, call!(alt((
						alt((  nullValue
							  , booleanValue
							  , guidValue
							  , dateValue
							  , dateTimeOffsetValue
							  , timeOfDayValue
							  , decimalValue
							  , doubleValue
							  , singleValue
							  , sbyteValue
							  , byteValue
							  , int16Value
							  , int32Value
							  , int64Value
							  , string
							  , duration
							  , _enum
						)),
						alt((
							  binary
							  , geographyCollection
							  , geographyLineString
							  , geographyMultiLineString
							  , geographyMultiPoint
							  , geographyMultiPolygon
							  , geographyPoint
							  , geographyPolygon
							  , geometryCollection
							  , geometryLineString
							  , geometryMultiLineString
							  , geometryMultiPoint
							  , geometryMultiPolygon
							  , geometryPoint
							  , geometryPolygon
						))
					))));
fn primitiveLiteral_wip<'a>(input: Input<'a>) -> IResult<Input<'a>, ast::Lit, Error> {
    alt((
        alt((
            // XXX make sure these are ordered in a away that eliminates ambiguity
            // We could also make sure that the input passed to this function contains only one
            // literal and then use the complete() combinator for all the cases bellow
            nullValue_wip,
            booleanValue_wip,
            guidValue_wip,
            dateTimeOffsetValue_wip,
            dateValue_wip,
            // timeOfDayValue_wip,
            // decimalValue_wip,
            // doubleValue_wip,
            // singleValue_wip,
            // sbyteValue_wip,
            // byteValue_wip,
            // int16Value_wip,
            // int32Value_wip,
            // int64Value_wip,
            // string_wip,
            // duration_wip,
            // _enum_wip,
        )),
        alt((
            value(ast::Lit::Unimplemented, binary),
            value(ast::Lit::Unimplemented, geographyCollection),
            value(ast::Lit::Unimplemented, geographyLineString),
            value(ast::Lit::Unimplemented, geographyMultiLineString),
            value(ast::Lit::Unimplemented, geographyMultiPoint),
            value(ast::Lit::Unimplemented, geographyMultiPolygon),
            value(ast::Lit::Unimplemented, geographyPoint),
            value(ast::Lit::Unimplemented, geographyPolygon),
            value(ast::Lit::Unimplemented, geometryCollection),
            value(ast::Lit::Unimplemented, geometryLineString),
            value(ast::Lit::Unimplemented, geometryMultiLineString),
            value(ast::Lit::Unimplemented, geometryMultiPoint),
            value(ast::Lit::Unimplemented, geometryMultiPolygon),
            value(ast::Lit::Unimplemented, geometryPoint),
            value(ast::Lit::Unimplemented, geometryPolygon),
        )),
    ))(input)
}
//*
//* ; in Atom and JSON message bodies and CSDL DefaultValue attributes
//* primitiveValue = booleanValue
//*                / guidValue
//*                / durationValue
//*                / dateValue
//*                / dateTimeOffsetValue
//*                / timeOfDayValue
//*                / enumValue
//*                / fullCollectionLiteral
//*                / fullLineStringLiteral
//*                / fullMultiPointLiteral
//*                / fullMultiLineStringLiteral
//*                / fullMultiPolygonLiteral
//*                / fullPointLiteral
//*                / fullPolygonLiteral
//*                / decimalValue
//*                / doubleValue
//*                / singleValue
//*                / sbyteValue
//*                / byteValue
//*                / int16Value
//*                / int32Value
//*                / int64Value
//*                / binaryValue
//*                ; also valid are:
//*                ; - any XML string for strings in Atom and CSDL documents
//*                ; - any JSON string for JSON documents
//TODO(XML & JSON strings)
named!(primitiveValue<Input, Input, Error>, call!(alt((
						alt(( booleanValue
						       , guidValue
						       , durationValue
						       , dateValue
						       , dateTimeOffsetValue
						       , timeOfDayValue
						       , enumValue
						       , fullCollectionLiteral
						       , fullLineStringLiteral
						       , fullMultiPointLiteral
						       , fullMultiLineStringLiteral
						       , fullMultiPolygonLiteral
						)),
						alt((
						       fullPointLiteral
						       , fullPolygonLiteral
						       , decimalValue
						       , doubleValue
						       , singleValue
						       , sbyteValue
						       , byteValue
						       , int16Value
						       , int32Value
						       , int64Value
						       , binaryValue
						))
					))));
//*
//* nullValue = 'null'
named!(nullValue<Input, Input, Error>, call!(tag("null")));
fn nullValue_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    value(ast::Lit::Null, tag("null"))(input)
}
//*
//* ; base64url encoding according to http://tools.ietf.org/html/rfc4648#section-5
//* binary      = "binary" SQUOTE binaryValue SQUOTE
named!(binary<Input, Input, Error>, call!(recognize(tuple((tag_no_case("binary"), SQUOTE, binaryValue, SQUOTE)))));
fn binary_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    preceded(
        tag_no_case("binary"),
        delimited(
            SQUOTE,
            map(binaryValue, |b| ast::Lit::Binary(b.to_string())),
            SQUOTE,
        ),
    )(input)
}
//* binaryValue = *(4base64char) [ base64b16  / base64b8 ]
named!(binaryValue<Input, Input, Error>, call!(recognize(tuple((many0(many_m_n(4, 4, base64char)), opt(alt((base64b16, base64b8))))))));
//* base64b16   = 2base64char ( 'A' / 'E' / 'I' / 'M' / 'Q' / 'U' / 'Y' / 'c' / 'g' / 'k' / 'o' / 's' / 'w' / '0' / '4' / '8' )   [ "=" ]
named!(base64b16<Input, Input, Error>, call!(recognize(tuple((many_m_n(2, 2, base64char), one_of("AEIMQUYcgkosw048"), opt(tag("=")))))));
//* base64b8    = base64char ( 'A' / 'Q' / 'g' / 'w' ) [ "==" ]
named!(base64b8<Input, Input, Error>, call!(recognize(tuple((base64char, one_of("AQgw"), opt(tag("==")))))));
//* base64char  = ALPHA / DIGIT / "-" / "_"
named!(base64char<Input, Input, Error>, call!(alt((ALPHA, DIGIT, tag("-"), tag("_")))));
//*
//* booleanValue = "true" / "false"
named!(booleanValue<Input, Input, Error>, call!(alt((tag_no_case("true"), tag_no_case("false")))));
fn booleanValue_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    alt((
        value(ast::Lit::Boolean(true), tag_no_case("true")),
        value(ast::Lit::Boolean(false), tag_no_case("false")),
    ))(input)
}
//*
//* decimalValue = [ SIGN ] 1*DIGIT [ "." 1*DIGIT ] [ "e" [ SIGN ] 1*DIGIT ] / nanInfinity
named!(decimalValue<Input, Input, Error>, call!(alt((recognize(tuple((opt(SIGN),
							many1(DIGIT),
						   	opt(tuple((tag("."), many1(DIGIT)))),
						   	opt(tuple((tag_no_case("e"), opt(SIGN), many1(DIGIT))))
							)))
				      , nanInfinity))));
//* doubleValue  = decimalValue ; IEEE 754 binary64 floating-point number (15-17 decimal digits)
named!(doubleValue<Input, Input, Error>, call!(recognize(decimalValue)));
//* singleValue  = decimalValue ; IEEE 754 binary32 floating-point number (6-9 decimal digits)
named!(singleValue<Input, Input, Error>, call!(recognize(decimalValue)));
//* nanInfinity  = 'NaN' / '-INF' / 'INF'
named!(nanInfinity<Input, Input, Error>, call!(alt((tag("NaN"), tag("-INF"), tag("INF")))));
//*
//* guidValue = 8HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 12HEXDIG
named!(guidValue<Input, Input, Error>, call!(recognize(tuple((many_m_n(8, 8, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(12, 12, HEXDIG)
						)))));
fn guidValue_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    map(
        map_res(take(uuid::adapter::Hyphenated::LENGTH), |res: Input| {
            Uuid::from_str(res.data)
        }),
        ast::Lit::Guid,
    )(input)
}
//*
//* byteValue  = 1*3DIGIT           ; numbers in the range from 0 to 255
named!(byteValue<Input, Input, Error>, call!(recognize(many_m_n(1, 3, DIGIT))));
//* sbyteValue = [ SIGN ] 1*3DIGIT  ; numbers in the range from -128 to 127
named!(sbyteValue<Input, Input, Error>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 3, DIGIT))))));
//* int16Value = [ SIGN ] 1*5DIGIT  ; numbers in the range from -32768 to 32767
named!(int16Value<Input, Input, Error>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 5, DIGIT))))));
//* int32Value = [ SIGN ] 1*10DIGIT ; numbers in the range from -2147483648 to 2147483647
named!(int32Value<Input, Input, Error>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 10, DIGIT))))));
//* int64Value = [ SIGN ] 1*19DIGIT ; numbers in the range from -9223372036854775808 to 9223372036854775807
named!(int64Value<Input, Input, Error>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 19, DIGIT))))));
//*
//* string           = SQUOTE *( SQUOTE-in-string / pchar-no-SQUOTE ) SQUOTE
// errata: pchar-no-SQUOTE includes special characters like &, =, and $. Those should be encoded
named!(string<Input, Input, Error>, call!(recognize(tuple((SQUOTE, many0(alt((recognize(SQUOTE_in_string), recognize(pchar_no_SQUOTE)))), SQUOTE)))));
//* SQUOTE-in-string = SQUOTE SQUOTE ; two consecutive single quotes represent one within a string literal
named!(SQUOTE_in_string<Input, Input, Error>, call!(recognize(tuple((SQUOTE, SQUOTE)))));
//*
//* dateValue = year "-" month "-" day
named!(dateValue<Input, Input, Error>, call!(recognize(tuple((year, tag("-"), month, tag("-"), day)))));
fn dateValue_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    let (input, (year, _, month, _, day)) =
        tuple((year_wip, tag("-"), month_wip, tag("-"), day_wip))(input)?;
    Ok((input, ast::Lit::Date(year, month, day)))
}
//*
//* dateTimeOffsetValue = year "-" month "-" day "T" hour ":" minute [ ":" second [ "." fractionalSeconds ] ] ( "Z" / SIGN hour ":" minute )
named!(dateTimeOffsetValue<Input, Input, Error>, call!(recognize(tuple((year, tag("-"), month, tag("-"), day, tag_no_case("T"), hour, tag(":"), minute,
							  opt(tuple((tag(":"), second, opt(tuple((tag("."), fractionalSeconds)))))),
							  alt((tag_no_case("Z"), recognize(tuple((SIGN, hour, tag(":"), minute)))))
							  )))));
fn dateTimeOffsetValue_wip(input: Input) -> IResult<Input, ast::Lit, Error> {
    let (input, (year, _, month, _, day)) =
        tuple((year_wip, tag("-"), month_wip, tag("-"), day_wip))(input)?;
    let (input, (_, hour, _, minute)) = tuple((tag("T"), hour_wip, tag(":"), minute_wip))(input)?;

    // FIXME correctly parse datetime offsets
    Ok((
        input,
        ast::Lit::DateTimeOffset(year, month, day, hour, minute),
    ))
}

//*
//* duration      = [ "duration" ] SQUOTE durationValue SQUOTE
named!(duration<Input, Input, Error>, call!(recognize(tuple((opt(tag_no_case("duration")), SQUOTE, durationValue, SQUOTE)))));
//* durationValue = [ SIGN ] "P" [ 1*DIGIT "D" ] [ "T" [ 1*DIGIT "H" ] [ 1*DIGIT "M" ] [ 1*DIGIT [ "." 1*DIGIT ] "S" ] ]
//*      ; the above is an approximation of the rules for an xml dayTimeDuration.
//*      ; see the lexical representation for dayTimeDuration in http://www.w3.org/TR/xmlschema11-2#dayTimeDuration for more information
named!(durationValue<Input, Input, Error>, call!(recognize(tuple((opt(SIGN),
						    tag_no_case("P"),
						    opt(tuple((many1(DIGIT), tag_no_case("D")))),
						    opt(tuple((tag_no_case("T"),
								opt(tuple((many1(DIGIT), tag_no_case("H")))),
								opt(tuple((many1(DIGIT), tag_no_case("M")))),
								opt(tuple((many1(DIGIT), opt(tuple((tag("."), many1(DIGIT)))), tag_no_case("S"))))
								))))))));

//*
//* timeOfDayValue = hour ":" minute [ ":" second [ "." fractionalSeconds ] ]
named!(timeOfDayValue<Input, Input, Error>, call!(recognize(tuple((hour, tag(":"), minute, opt(tuple((tag(":"), second, opt(tuple((tag("."), fractionalSeconds)))))))))));
//*
//* oneToNine       = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
named!(oneToNine<Input, Input, Error>, call!(recognize(one_of("123456789"))));
//* zeroToFiftyNine = ( "0" / "1" / "2" / "3" / "4" / "5" ) DIGIT
named!(zeroToFiftyNine<Input, Input, Error>, call!(recognize(tuple((one_of("012345"), DIGIT)))));
//* year  = [ "-" ] ( "0" 3DIGIT / oneToNine 3*DIGIT )
named!(year<Input, Input, Error>, call!(recognize(tuple((opt(tag("-")), alt((recognize(tuple((tag("0"), many_m_n(3, 3, DIGIT)))), recognize(tuple((oneToNine, many_m_n(3, 3, DIGIT)))))))))));
fn year_wip(input: Input) -> IResult<Input, i16, Error> {
    map_res(
        recognize(tuple((
            opt(tag("-")),
            take_while_m_n(4, 4, |c: char| c.is_digit(10)),
        ))),
        |res: Input| i16::from_str(res.data),
    )(input)
}

//* month = "0" oneToNine
//*       / "1" ( "0" / "1" / "2" )
named!(month<Input, Input, Error>, call!(alt((  recognize(tuple((tag("0"), oneToNine)))
			       , recognize(tuple((tag("1"), one_of("012"))))
			       ))));
fn month_wip(input: Input) -> IResult<Input, u8, Error> {
    verify(
        map_res(
            take_while_m_n(2, 2, |c: char| c.is_digit(10)),
            |res: Input| u8::from_str(res.data),
        ),
        |m| (1 <= *m && *m <= 12),
    )(input)
}
//* day   = "0" oneToNine
//*       / ( "1" / "2" ) DIGIT
//*       / "3" ( "0" / "1" )
named!(day<Input, Input, Error>, call!(alt((  recognize(tuple((tag("0"), oneToNine)))
			     , recognize(tuple((one_of("12"), DIGIT)))
			     , recognize(tuple((tag("3"), one_of("01"))))
			     ))));
fn day_wip(input: Input) -> IResult<Input, u8, Error> {
    verify(
        map_res(
            take_while_m_n(2, 2, |c: char| c.is_digit(10)),
            |res: Input| u8::from_str(res.data),
        ),
        |m| (1 <= *m && *m <= 31),
    )(input)
}
//* hour   = ( "0" / "1" ) DIGIT
//*        / "2" ( "0" / "1" / "2" / "3" )
named!(hour<Input, Input, Error>, call!(alt((  recognize(tuple((one_of("01"), DIGIT)))
			      , recognize(tuple((tag("2"), one_of("0123"))))
			      ))));
fn hour_wip(input: Input) -> IResult<Input, u8, Error> {
    verify(
        map_res(
            take_while_m_n(2, 2, |c: char| c.is_digit(10)),
            |res: Input| u8::from_str(res.data),
        ),
        |m| *m <= 23,
    )(input)
}
//* minute = zeroToFiftyNine
named!(minute<Input, Input, Error>, call!(recognize(zeroToFiftyNine)));
fn minute_wip(input: Input) -> IResult<Input, u8, Error> {
    verify(
        map_res(
            take_while_m_n(2, 2, |c: char| c.is_digit(10)),
            |res: Input| u8::from_str(res.data),
        ),
        |m| *m <= 59,
    )(input)
}
//* second = zeroToFiftyNine
named!(second<Input, Input, Error>, call!(recognize(zeroToFiftyNine)));
fn second_wip(input: Input) -> IResult<Input, u8, Error> {
    verify(
        map_res(
            take_while_m_n(2, 2, |c: char| c.is_digit(10)),
            |res: Input| u8::from_str(res.data),
        ),
        |m| *m <= 59,
    )(input)
}
//* fractionalSeconds = 1*12DIGIT
named!(fractionalSeconds<Input, Input, Error>, call!(recognize(many_m_n(1, 12, DIGIT))));
//*
//* enum            = [ qualifiedEnumTypeName ] SQUOTE enumValue SQUOTE
named!(_enum<Input, Input, Error>, call!(recognize(tuple((opt(qualifiedEntityTypeName), SQUOTE, enumValue, SQUOTE)))));
//* enumValue       = singleEnumValue *( COMMA singleEnumValue )
named!(enumValue<Input, Input, Error>, call!(recognize(tuple((singleEnumValue, many0(tuple((COMMA, singleEnumValue))))))));
//* singleEnumValue = enumerationMember / enumMemberValue
named!(singleEnumValue<Input, Input, Error>, call!(alt((enumerationMember, enumMemberValue))));
//* enumMemberValue = int64Value
named!(enumMemberValue<Input, Input, Error>, call!(recognize(int64Value)));

//* geographyCollection   = geographyPrefix SQUOTE fullCollectionLiteral SQUOTE
named!(geographyCollection<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)))));
//* fullCollectionLiteral = sridLiteral collectionLiteral
named!(fullCollectionLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, collectionLiteral)))));
//* collectionLiteral     = "Collection(" geoLiteral *( COMMA geoLiteral ) CLOSE
named!(collectionLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("Collection("), geoLiteral, many0(tuple((COMMA, geoLiteral))), CLOSE)))));
//* geoLiteral            = collectionLiteral
//*                       / lineStringLiteral
//*                       / multiPointLiteral
//*                       / multiLineStringLiteral
//*                       / multiPolygonLiteral
//*                       / pointLiteral
//*                       / polygonLiteral
named!(geoLiteral<Input, Input, Error>, call!(alt((  collectionLiteral
				    , lineStringLiteral
				    , multiPointLiteral
				    , multiLineStringLiteral
				    , multiPolygonLiteral
				    , pointLiteral
				    , polygonLiteral))));
//*
//* geographyLineString   = geographyPrefix SQUOTE fullLineStringLiteral SQUOTE
named!(geographyLineString<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)))));
//* fullLineStringLiteral = sridLiteral lineStringLiteral
named!(fullLineStringLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, lineStringLiteral)))));
//* lineStringLiteral     = "LineString" lineStringData
named!(lineStringLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("LineString"), lineStringData)))));
//* lineStringData        = OPEN positionLiteral 1*( COMMA positionLiteral ) CLOSE
named!(lineStringData<Input, Input, Error>, call!(recognize(tuple((OPEN, positionLiteral, many1(tuple((COMMA, positionLiteral))), CLOSE)))));
//*
//* geographyMultiLineString   = geographyPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geographyMultiLineString<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)))));
//* fullMultiLineStringLiteral = sridLiteral multiLineStringLiteral
named!(fullMultiLineStringLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, multiLineStringLiteral)))));
//* multiLineStringLiteral     = "MultiLineString(" [ lineStringData *( COMMA lineStringData ) ] CLOSE
named!(multiLineStringLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("MultiLineString("), opt(tuple((lineStringData, many0(tuple((COMMA, lineStringData)))))), CLOSE)))));
//*
//* geographyMultiPoint   = geographyPrefix SQUOTE fullMultiPointLiteral SQUOTE
named!(geographyMultiPoint<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)))));
//* fullMultiPointLiteral = sridLiteral multiPointLiteral
named!(fullMultiPointLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, multiPointLiteral)))));
//* multiPointLiteral     = "MultiPoint(" [ pointData *( COMMA pointData ) ] CLOSE
named!(multiPointLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("MultiPoint("), opt(tuple((pointData, many0(tuple((COMMA, pointData)))))), CLOSE)))));
//*
//* geographyMultiPolygon   = geographyPrefix SQUOTE fullMultiPolygonLiteral SQUOTE
named!(geographyMultiPolygon<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)))));
//* fullMultiPolygonLiteral = sridLiteral multiPolygonLiteral
named!(fullMultiPolygonLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, multiPolygonLiteral)))));
//* multiPolygonLiteral     = "MultiPolygon(" [ polygonData *( COMMA polygonData ) ] CLOSE
named!(multiPolygonLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("MultiPolygon("), opt(tuple((polygonData, many0(tuple((COMMA, polygonData)))))), CLOSE)))));
//*
//* geographyPoint   = geographyPrefix SQUOTE fullPointLiteral SQUOTE
named!(geographyPoint<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullPointLiteral, SQUOTE)))));
//* fullPointLiteral = sridLiteral pointLiteral
named!(fullPointLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, pointLiteral)))));
//* sridLiteral      = "SRID" EQ 1*5DIGIT SEMI
named!(sridLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("SRID"), EQ, many_m_n(1, 5, DIGIT), SEMI)))));
//* pointLiteral     ="Point" pointData
named!(pointLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("Point"), pointData)))));
//* pointData        = OPEN positionLiteral CLOSE
named!(pointData<Input, Input, Error>, call!(recognize(tuple((OPEN, positionLiteral, CLOSE)))));
//* positionLiteral  = doubleValue SP doubleValue  ; longitude, then latitude
named!(positionLiteral<Input, Input, Error>, call!(recognize(tuple((doubleValue, SP, doubleValue)))));
//*
//* geographyPolygon   = geographyPrefix SQUOTE fullPolygonLiteral SQUOTE
named!(geographyPolygon<Input, Input, Error>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)))));
//* fullPolygonLiteral = sridLiteral polygonLiteral
named!(fullPolygonLiteral<Input, Input, Error>, call!(recognize(tuple((sridLiteral, polygonLiteral)))));
//* polygonLiteral     = "Polygon" polygonData
named!(polygonLiteral<Input, Input, Error>, call!(recognize(tuple((tag_no_case("Polygon"), polygonData)))));
//* polygonData        = OPEN ringLiteral *( COMMA ringLiteral ) CLOSE
named!(polygonData<Input, Input, Error>, call!(recognize(tuple((OPEN, ringLiteral, many0(tuple((COMMA, ringLiteral))), CLOSE)))));
//* ringLiteral        = OPEN positionLiteral *( COMMA positionLiteral ) CLOSE
//*                    ; Within each ringLiteral, the first and last positionLiteral elements MUST be an exact syntactic match to each other.
//*                    ; Within the polygonData, the ringLiterals MUST specify their points in appropriate winding order.
//*                    ; In order of traversal, points to the left side of the ring are interpreted as being in the polygon.
named!(ringLiteral<Input, Input, Error>, call!(recognize(tuple((OPEN, positionLiteral, many0(tuple((COMMA, positionLiteral))), CLOSE)))));
//*
//* geometryCollection      = geometryPrefix SQUOTE fullCollectionLiteral      SQUOTE
named!(geometryCollection<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)))));
//* geometryLineString      = geometryPrefix SQUOTE fullLineStringLiteral      SQUOTE
named!(geometryLineString<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)))));
//* geometryMultiLineString = geometryPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geometryMultiLineString<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)))));
//* geometryMultiPoint      = geometryPrefix SQUOTE fullMultiPointLiteral      SQUOTE
named!(geometryMultiPoint<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)))));
//* geometryMultiPolygon    = geometryPrefix SQUOTE fullMultiPolygonLiteral    SQUOTE
named!(geometryMultiPolygon<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)))));
//* geometryPoint           = geometryPrefix SQUOTE fullPointLiteral           SQUOTE
named!(geometryPoint<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullPointLiteral, SQUOTE)))));
//* geometryPolygon         = geometryPrefix SQUOTE fullPolygonLiteral         SQUOTE
named!(geometryPolygon<Input, Input, Error>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)))));
//*
//* geographyPrefix = "geography"
named!(geographyPrefix<Input, Input, Error>, call!(tag_no_case("geography")));
//* geometryPrefix  = "geometry"
named!(geometryPrefix<Input, Input, Error>, call!(tag_no_case("geometry")));
//*
//*

//TODO(header)
//* ;------------------------------------------------------------------------------
//* ; 8. Header values
//* ;------------------------------------------------------------------------------
//*
//* header = content-id
//*        / entityid
//*        / isolation
//*        / odata-maxversion
//*        / odata-version
//*        / prefer
//*
//* content-id = "Content-ID" ":" OWS request-id
//* request-id = 1*unreserved
//*
//* entityid   = [ "OData-" ] "EntityID" ":" OWS IRI-in-header
//* isolation  = [ "OData-" ] "Isolation" ":" OWS "snapshot"
//* odata-maxversion = "OData-MaxVersion" ":" OWS 1*DIGIT "." 1*DIGIT
//* odata-version    = "OData-Version"    ":" OWS "4.0" [ oneToNine ]
//*
//* prefer     = "Prefer" ":" OWS preference *( COMMA preference )
//* preference = allowEntityReferencesPreference
//*            / callbackPreference
//*            / continueOnErrorPreference
//*            / includeAnnotationsPreference
//*            / maxpagesizePreference
//*            / respondAsyncPreference
//*            / returnPreference
//*            / trackChangesPreference
//*            / waitPreference
//*            ; and everything allowed by http://tools.ietf.org/html/draft-snell-http-prefer-18
//*            ; / token [ EQ-h word ] *( OWS ";" [ OWS parameter ] )
//*
//* allowEntityReferencesPreference = [ "odata." ] "allow-entityreferences"
//*
//* callbackPreference = [ "odata." ] "callback" OWS ";" OWS "url" EQ-h DQUOTE URI DQUOTE
//*
//* continueOnErrorPreference = [ "odata." ] "continue-on-error" [ EQ-h booleanValue ]
//*
//* includeAnnotationsPreference = [ "odata." ] "include-annotations" EQ-h DQUOTE annotationsList DQUOTE
//* annotationsList      = annotationIdentifier *(COMMA annotationIdentifier)
//* annotationIdentifier = [ excludeOperator ]
//*                        ( STAR
//*                        / namespace "." ( termName / STAR )
//*                        )
//*                        [ "#" odataIdentifier ]
//* excludeOperator      = "-"
//*
//* maxpagesizePreference = [ "odata." ] "maxpagesize" EQ-h oneToNine *DIGIT
//*
//* respondAsyncPreference = "respond-async"
//*
//* returnPreference = "return" EQ-h ( 'representation' / 'minimal' )
//*
//* trackChangesPreference = [ "odata." ] "track-changes"
//*
//* waitPreference = "wait" EQ-h 1*DIGIT
//*
//* ;parameter      = token [ EQ-h word ]
//* ;word           = token / quoted-string
//* ;token          = 1*tchar
//* ;tchar          = "!" / "#" / "$" / "%" / "&" / "'" / "*"
//* ;               / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
//* ;               / DIGIT / ALPHA
//* ;quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
//* ;qdtext         = %x21 / %x23-5B / %x5D-7E / obs-text / OWS
//* obs-text       = %x80-FF
named!(obs_text<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && (*chr as u8) >= 0x80))));
//* ;quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
//*
//* OWS   = *( SP / HTAB )  ; "optional" whitespace
named!(OWS<Input, Input, Error>, call!(recognize(many0(alt((SP, HTAB))))));
//* BWS-h = *( SP / HTAB )  ; "bad" whitespace in header values
named!(BWS_h<Input, Input, Error>, call!(recognize(many0(alt((SP, HTAB))))));
//* EQ-h  = BWS-h EQ BWS-h
named!(EQ_h<Input, Input, Error>, call!(recognize(tuple((BWS_h, EQ, BWS_h)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 9. Punctuation
//* ;------------------------------------------------------------------------------
//*
//* RWS = 1*( SP / HTAB / "%20" / "%09" )  ; "required" whitespace
named!(RWS<Input, Input, Error>, call!(recognize(many1(alt((SP, HTAB, tag("%20"), tag("%09")))))));
//* BWS =  *( SP / HTAB / "%20" / "%09" )  ; "bad" whitespace
named!(BWS<Input, Input, Error>, call!(recognize(many0(alt((SP, HTAB, tag("%20"), tag("%09")))))));
//*
//* AT     = "@" / "%40"
named!(AT<Input, Input, Error>, call!(alt((tag("@"), tag("%40")))));
//* COLON  = ":" / "%3A"
named!(COLON<Input, Input, Error>, call!(alt((tag(":"), tag("%3A")))));
//* COMMA  = "," / "%2C"
named!(COMMA<Input, Input, Error>, call!(alt((tag(","), tag("%2C")))));
//* EQ     = "="
named!(EQ<Input, Input, Error>, call!(tag("=")));
//* SIGN   = "+" / "%2B" / "-"
named!(SIGN<Input, Input, Error>, call!(alt((tag("+"), tag("%3B"), tag("-")))));
//* SEMI   = ";" / "%3B"
named!(SEMI<Input, Input, Error>, call!(alt((tag(";"), tag("%3B")))));
//* STAR   = "*" / "%2A"
named!(STAR<Input, Input, Error>, call!(alt((tag("*"), tag("%2A")))));
//* SQUOTE = "'" / "%27"
named!(SQUOTE<Input, Input, Error>, call!(alt((tag("'"), tag("%27")))));
//*
//* OPEN  = "(" / "%28"
named!(OPEN<Input, Input, Error>, call!(alt((tag("("), tag("%28")))));
//* CLOSE = ")" / "%29"
named!(CLOSE<Input, Input, Error>, call!(alt((tag(")"), tag("%29")))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; A. URI syntax [RFC3986]
//* ;------------------------------------------------------------------------------
//*
//* URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
named!(URI<Input, Input, Error>, call!(recognize(tuple((scheme, tag(":"), hier_part, opt(tuple((tag("?"), query))), opt(tuple((tag("#"), fragment))))))));
//* hier-part     = "//" authority path-abempty
//*               / path-absolute
//*               / path-rootless
//* ;              / path-empty
named!(hier_part<Input, Input, Error>, call!(recognize(tuple((tag("//"), authority, alt((path_abempty, path_absolute, path_rootless, path_empty)))))));
//* ;URI-reference = URI / relative-ref
//* ;absolute-URI  = scheme ":" hier-part [ "?" query ]
//* ;relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
//* ;relative-part = "//" authority path-abempty
//* ;              / path-absolute
//* ;              / path-noscheme
//* ;              / path-empty
//* scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
named!(scheme<Input, Input, Error>, call!(recognize(tuple((ALPHA, many0(alt((ALPHA, DIGIT, tag("+"), tag("-"), tag(".")))))))));
//* authority     = [ userinfo "@" ] host [ ":" port ]
named!(authority<Input, Input, Error>, call!(recognize(tuple((opt(tuple((userinfo, tag("@")))), host, opt(tuple((tag(":"), port))))))));
//* userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
named!(userinfo<Input, Input, Error>, call!(recognize(many0(alt((unreserved, pct_encoded, sub_delims, tag(":")))))));
//* host          = IP-literal / IPv4address / reg-name
named!(host<Input, Input, Error>, call!(alt((IP_literal, IPv4address, reg_name))));
//* port          = *DIGIT
named!(port<Input, Input, Error>, call!(recognize(many0(DIGIT))));
//* IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
named!(IP_literal<Input, Input, Error>, call!(delimited(tag("["), alt((IPv6address, IPvFuture)), tag("]"))));
//* IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
named!(IPvFuture<Input, Input, Error>, call!(recognize(tuple((tag("v"), many1(HEXDIG), tag("."), many1(alt((unreserved, sub_delims, tag(":")))))))));
//* IPv6address   =                            6( h16 ":" ) ls32
//*                  /                       "::" 5( h16 ":" ) ls32
//*                  / [               h16 ] "::" 4( h16 ":" ) ls32
//*                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
//*                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
//*                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
//*                  / [ *4( h16 ":" ) h16 ] "::"              ls32
//*                  / [ *5( h16 ":" ) h16 ] "::"              h16
//*                  / [ *6( h16 ":" ) h16 ] "::"
named!(IPv6address<Input, Input, Error>, call!(alt((
		recognize(tuple((                                                                        many_m_n(6, 6, tuple((h16, tag(":")))), ls32))) ,
		recognize(tuple((                                                            tag("::"), many_m_n(5, 5, tuple((h16, tag(":")))), ls32))) ,
		recognize(tuple((opt(                                                h16 ), tag("::"), many_m_n(4, 4, tuple((h16, tag(":")))), ls32))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 1, tuple((h16, tag(":")))), h16))), tag("::"), many_m_n(3, 3, tuple((h16, tag(":")))), ls32))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 2, tuple((h16, tag(":")))), h16))), tag("::"), many_m_n(2, 2, tuple((h16, tag(":")))), ls32))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 3, tuple((h16, tag(":")))), h16))), tag("::"),                        h16, tag(":"),   ls32))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 4, tuple((h16, tag(":")))), h16))), tag("::"),                                          ls32))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 5, tuple((h16, tag(":")))), h16))), tag("::"),                                           h16))) ,
		recognize(tuple((opt(tuple((many_m_n(0, 6, tuple((h16, tag(":")))), h16))), tag("::")                                               )))
))));
//* h16           = 1*4HEXDIG
named!(h16<Input, Input, Error>, call!(recognize(many1(many_m_n(4, 4, HEXDIG)))));
//* ls32          = ( h16 ":" h16 ) / IPv4address
named!(ls32<Input, Input, Error>, call!(alt((recognize(separated_pair(h16, tag(":"), h16)), IPv4address))));
//* IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
named!(IPv4address<Input, Input, Error>, call!(recognize(tuple((dec_octet, tag("."), dec_octet, tag("."), dec_octet, tag("."), dec_octet)))));
//* dec-octet     = "1" 2DIGIT            ; 100-199
//*               / "2" %x30-34 DIGIT     ; 200-249
//*               / "25" %x30-35          ; 250-255
//*               / %x31-39 DIGIT         ; 10-99
//*               / DIGIT                 ; 0-9
named!(dec_octet<Input, Input, Error>, call!(alt((
	recognize(tuple((tag("1"), DIGIT, DIGIT)))            ,
	recognize(tuple((tag("2"), one_of("01234"), DIGIT))) ,
	recognize(tuple((tag("25"), one_of("012345"))))      ,
	recognize(tuple((one_of("123456789"), DIGIT)))        ,
	DIGIT
))));
//* reg-name      = *( unreserved / pct-encoded / sub-delims )
named!(reg_name<Input, Input, Error>, call!(recognize(many0(alt((unreserved, pct_encoded, sub_delims))))));
//* ;path          = path-abempty    ; begins with "/" or is empty
//* ;              / path-absolute   ; begins with "/" but not "//"
//* ;              / path-noscheme   ; begins with a non-colon segment
//* ;              / path-rootless   ; begins with a segment
//* ;              / path-empty      ; zero characters
//* path-abempty  = *( "/" segment )
named!(path_abempty<Input, Input, Error>, call!(recognize(many0(preceded(tag("/"), segment)))));
//* path-absolute = "/" [ segment-nz *( "/" segment ) ]
named!(path_absolute<Input, Input, Error>, call!(recognize(tuple((tag("/"), opt(tuple((segment_nz, path_abempty))))))));
//* ;path-noscheme = segment-nz-nc *( "/" segment )
//* path-rootless = segment-nz *( "/" segment )
named!(path_rootless<Input, Input, Error>, call!(recognize(tuple((segment_nz, path_abempty)))));
//* ;path-empty    = ""
named!(path_empty<Input, Input, Error>, call!(tag("")));
//* segment       = *pchar
named!(segment<Input, Input, Error>, call!(recognize(many0(pchar))));
//* segment-nz    = 1*pchar
named!(segment_nz<Input, Input, Error>, call!(recognize(many1(pchar))));
//* ;segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
//* pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
named!(pchar<Input, Input, Error>, call!(alt((unreserved, pct_encoded, sub_delims, recognize(one_of(":@"))))));
//* query         = *( pchar / "/" / "?" )
named!(query<Input, Input, Error>, call!(recognize(many0(alt((pchar, recognize(one_of("/?"))))))));
//* fragment      = *( pchar / "/" / "?" )
named!(fragment<Input, Input, Error>, call!(recognize(many0(alt((pchar, recognize(one_of("/?"))))))));
//* pct-encoded   = "%" HEXDIG HEXDIG
named!(pct_encoded<Input, Input, Error>, call!(recognize(tuple((tag("%"), HEXDIG, HEXDIG)))));
//* unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
named!(unreserved<Input, Input, Error>, call!(alt((ALPHA, DIGIT, recognize(one_of("-._~"))))));
//* ;reserved      = gen-delims / sub-delims
//* ;gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
//* ;sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
//* sub-delims     =       "$" / "&" / "'" /                                     "=" / other-delims
named!(sub_delims<Input, Input, Error>, call!(alt((recognize(one_of("$&'=")), other_delims))));
//* other-delims   = "!" /                   "(" / ")" / "*" / "+" / "," / ";"
named!(other_delims<Input, Input, Error>, call!(recognize(one_of("!()*+,;"))));
//*
//* pchar-no-SQUOTE       = unreserved / pct-encoded-no-SQUOTE / other-delims / "$" / "&" / "=" / ":" / "@"
named!(pchar_no_SQUOTE<Input, Input, Error>, call!(alt((unreserved, pct_encoded_no_SQUOTE, other_delims, recognize(one_of("$&=:@"))))));
//* pct-encoded-no-SQUOTE = "%" ( "0" / "1" /   "3" / "4" / "5" / "6" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" / "2" / "3" / "4" / "5" / "6" /   "8" / "9" / A-to-F )
named!(pct_encoded_no_SQUOTE<Input, Input, Error>, call!(alt((  recognize(tuple((tag("%"), one_of("013456789ABCDEFabcdef"), HEXDIG)))
					       , recognize(tuple((tag("%2"), one_of("012345689ABCDEFabcdef"))))
					      ))));
//*
//* qchar-no-AMP              = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_no_AMP<Input, Input, Error>, call!(alt((qchar_no_AMP_EQ_AT_DOLLAR, tag("=")))));
//* qchar-no-AMP-EQ           = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'"
named!(qchar_no_AMP_EQ<Input, Input, Error>, call!(alt((qchar_no_AMP_EQ_AT_DOLLAR, tag("@"), tag("$")))));
//* qchar-no-AMP-EQ-AT-DOLLAR = unreserved / pct-encoded / other-delims / ":" /       "/" / "?" /       "'"
named!(qchar_no_AMP_EQ_AT_DOLLAR<Input, Input, Error>, call!(alt((unreserved, pct_encoded, other_delims, recognize(one_of(":/?'"))))));
//*
//* qchar-unescaped       = unreserved / pct-encoded-unescaped / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_unescaped<Input, Input, Error>, call!(alt((unreserved, pct_encoded_unscaped, other_delims, recognize(one_of(":@/?$'="))))));
//* pct-encoded-unescaped = "%" ( "0" / "1" /   "3" / "4" /   "6" / "7" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" /   "3" / "4" / "5" / "6" / "7" / "8" / "9" / A-to-F )
//*                       / "%" "5" ( DIGIT / "A" / "B" /   "D" / "E" / "F" )
named!(pct_encoded_unscaped<Input, Input, Error>, call!(alt(( recognize(tuple((tag("%"), alt((recognize(one_of("01346789")), A_to_F)), HEXDIG)))
					     , recognize(tuple((tag("%2"), alt((recognize(one_of("013456789")), A_to_F)))))
					     , recognize(tuple((tag("%5"), alt((DIGIT, recognize(one_of("ABDEFabdef")))))))
					     ))));

//*
//* qchar-no-AMP-DQUOTE   = qchar-unescaped
//*                       / escape ( escape / quotation-mark )
named!(qchar_no_AMP_DQUOTE<Input, Input, Error>, call!(alt((qchar_unescaped, recognize(tuple((escape, alt((escape, quotation_mark)))))))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; B. IRI syntax [RFC3987]
//* ;------------------------------------------------------------------------------
//* ; Note: these are over-generous stubs, for the actual patterns refer to RFC3987
//* ;------------------------------------------------------------------------------
//*
//* IRI-in-header = 1*( VCHAR / obs-text )
named!(IRI_in_header<Input, Input, Error>, call!(recognize(many1(alt((VCHAR, obs_text))))));
//* IRI-in-query  = 1*qchar-no-AMP
named!(IRI_in_query<Input, Input, Error>, call!(recognize(many1(qchar_no_AMP))));

//* ;------------------------------------------------------------------------------
//* ; C. ABNF core definitions [RFC5234]
//* ;------------------------------------------------------------------------------
//*
//* ALPHA  = %x41-5A / %x61-7A
named!(ALPHA<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_alphabetic(*chr as u8)))));

//* DIGIT  = %x30-39
named!(DIGIT<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_digit(*chr as u8)))));
//
// //* HEXDIG = DIGIT / A-to-F
named!(HEXDIG<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_hex_digit(*chr as u8)))));

//* A-to-F = "A" / "B" / "C" / "D" / "E" / "F"
fn is_A_to_F(chr: u8) -> bool {
    (chr >= 0x41 && chr <= 0x46) || (chr >= 0x61 && chr <= 0x66)
}
named!(A_to_F<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_A_to_F(*chr as u8)))));

//* DQUOTE = %x22
named!(DQUOTE<Input, Input, Error>, call!(tag("\u{0022}")));

//* SP     = %x20
named!(SP<Input, Input, Error>, call!(tag("\u{0020}")));

//* HTAB   = %x09
named!(HTAB<Input, Input, Error>, call!(tag("\u{0009}")));

//* ;WSP    = SP / HTAB
//* ;LWSP = *(WSP / CRLF WSP)
//* VCHAR = %x21-7E
named!(VCHAR<Input, Input, Error>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii_graphic()))));

//* ;CHAR = %x01-7F
//* ;LOCTET = %x00-FF
//* ;CR     = %x0D
//* ;LF     = %x0A
//* ;CRLF   = CR LF
//* ;BIT = "0" / "1"
//*
//*
//* ;------------------------------------------------------------------------------
//* ; End of odata-abnf-construction-rules
//* ;------------------------------------------------------------------------------
