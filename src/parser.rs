#![allow(non_snake_case)]
//FIXME
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::HashMap;
use std::iter::FromIterator;

use std::str;
use super::ast;
use super::schema;

use nom::{IResult,Err,Needed};

use nom::bytes::complete::*;
use nom::character::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;
use nom::branch::*;

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

pub struct Parser<'a> {
	document: &'a schema::Document,
	// bound_vars: RefCell<HashMap<Identifier, (Type, Option<Value>>>,
}

impl<'a> Parser<'a> {
	pub fn new(document: &'a schema::Document) -> Self {
		Parser{
			document: document,
		}
	}

	pub fn parse(&'a mut self, input: &'a str) -> IResult<&'a str, ast::ODataURI<'a>> {
		odataUri(input, self, self.document)
	}
}

pub fn odataUri<'a>(input: &'a str, ctx: &'a Parser, document: &'a schema::Document) -> IResult<&'a str, ast::ODataURI<'a>> {
	do_parse!(input,
		service_root: call!(serviceRoot, ctx, &document.service_root) >>
		relative_uri: call!(opt(|input| odataRelativeUri(input, ctx, &document.schema))) >>
		(ast::ODataURI{service_root, relative_uri})
	)
}
//*
//* serviceRoot = ( "https" / "http" )                    ; Note: case-insensitive
//*               "://" host [ ":" port ]
//*               "/" *( segment-nz "/" )
fn serviceRoot<'a>(input: &'a str, ctx: &Parser, service_root: &'a str) -> IResult<&'a str, &'a str> {
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
fn odataRelativeUri<'a>(input: &'a str, ctx: &'a Parser, schema: &'a schema::Schema)-> IResult<&'a str, ast::RelativeURI<'a>> {
	alt((
		map(preceded(tag("$batch"), opt(preceded(tag("?"), batchOptions))), ast::RelativeURI::Batch)
		, value(ast::RelativeURI::Entity, tuple((tag("$entity"), tag("?"), entityOptions)))
		, value(ast::RelativeURI::Entity, tuple((tag("$entity/"), qualifiedEntityTypeName, tag("?"), entityCastOptions)))
		, value(ast::RelativeURI::Metadata, tuple((tag("$metadata"), opt(tuple((tag("?"), metadataOptions))), opt(context))))
		, |input: &'a str| {
			do_parse!(input,
				segments: call!(resourcePath, ctx, schema.get_entity_container()) >>
				options: call!(opt(preceded(tag("?"), |i| queryOptions_wip(i, ctx)))) >>
				(ast::RelativeURI::Resource(ast::ResourcePath{segments, options}))
			)
		}
	))(input)
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
fn resourcePath<'a>(input: &'a str, ctx: &'a Parser, entity_container: &'a schema::EntityContainer) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	alt((
		|i: &'a str| {
			let (i, entity_set) = entitySetName_wip(i, ctx, entity_container)?;
			let (i, options) = opt(|i: &'a str| collectionNavigation_wip(i, ctx, &entity_set.kind))(i)?;

			let mut path = vec![ast::PathSegment::EntitySet(entity_set)];
			if let Some(mut options) = options {
				path.append(&mut options);
			}
			Ok((i, path))
		}
		, value(vec![ast::PathSegment::Singleton], recognize(tuple((singletonEntity, opt(singleNavigation)))))
		, value(vec![ast::PathSegment::Action], actionImportCall)
		, value(vec![ast::PathSegment::Function], recognize(tuple((entityColFunctionImportCall, opt(collectionNavigation)))))
		, value(vec![ast::PathSegment::Function], recognize(tuple((entityFunctionImportCall, opt(singleNavigation)))))
		, value(vec![ast::PathSegment::Function], recognize(tuple((complexColFunctionImportCall, opt(complexColPath)))))
		, value(vec![ast::PathSegment::Function], recognize(tuple((complexFunctionImportCall, opt(complexPath)))))
		, value(vec![ast::PathSegment::Function], recognize(tuple((primitiveColFunctionImportCall, opt(primitiveColPath)))))
		, value(vec![ast::PathSegment::Function], recognize(tuple((primitiveFunctionImportCall, opt(primitivePath)))))
		, value(vec![ast::PathSegment::Function], functionImportCallNoParens)
		, value(vec![ast::PathSegment::Crossjoin], crossjoin)
		, value(vec![ast::PathSegment::All], recognize(tuple((tag("$all"), opt(tuple((tag("/"), qualifiedEntityTypeName)))))))
	))(input)
}

//*
//* collectionNavigation = [ "/" qualifiedEntityTypeName ] [ collectionNavPath ]
named!(collectionNavigation<&str, &str>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(collectionNavPath))))));
fn collectionNavigation_wip<'a>(input: &'a str, ctx: &'a Parser, kind: &'a schema::kind::Entity) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	do_parse!(input,
		cast: call!(opt(preceded(tag("/"), qualifiedEntityTypeName))) >>
		path: call!(opt(|i| collectionNavPath_wip(i, ctx, kind))) >>
		({
			let mut result = vec![];
			if let Some(cast) = cast {
				result.push(ast::PathSegment::Cast);
			}
			if let Some(mut path) = path {
				result.append(&mut path);
			}
			result
		})
	)
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
named!(collectionNavPath<&str, &str>, call!(alt((recognize(tuple((keyPredicate, opt(singleNavigation))))
					   , recognize(tuple((filterInPath, opt(collectionNavigation))))
					   , recognize(tuple((each, opt(boundOperation))))
					   , boundOperation
					   , count
					   , _ref
					))));
fn collectionNavPath_wip<'a>(input: &'a str, ctx: &'a Parser, kind: &'a schema::kind::Entity) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	alt((
		|i| {
			let (i, filter) = filterInPath_wip(i)?;
			let (i, path) = opt(|i| collectionNavigation_wip(i, ctx, kind))(i)?;
			let mut result = vec![filter];
			if let Some(mut path) = path {
				result.append(&mut path);
			}
			Ok((i, result))
		},
		|i| {
			let (input, (each, bound_op)) = tuple((each_wip, opt(|i| boundOperation_wip(i, ctx))))(i)?;
			let mut result = vec![each];
			if let Some(mut bound_op) = bound_op {
				result.append(&mut bound_op);
			}

			Ok((input, result))
		},
		|i| boundOperation_wip(i, ctx),
		map(count_wip, |s| vec![s]),
		map(_ref_wip, |s| vec![s]),
		|i| {
			match &kind.key {
				Some(key) => {
					let (i, key) = keyPredicate_wip(i, ctx, &kind.properties, key)?;
					let (i, path) = opt(|i| singleNavigation_wip(i, ctx, kind))(i)?;

					let mut result = vec![ast::PathSegment::KeyPredicate(key)];
					if let Some(mut path) = path {
						result.append(&mut path);
					}
					Ok((i, result))
				},
				//FIXME find a way to do proper error handling
				None => Err(nom::Err::Error((i, nom::error::ErrorKind::Alt))),
			}
		},
	))(input)
}

//*
//* keyPredicate     = simpleKey / compoundKey / keyPathSegments
named!(keyPredicate<&str, &str>, call!(alt((simpleKey, compoundKey, keyPathSegments))));
fn keyPredicate_wip<'a>(input: &'a str, ctx: &Parser, props: &'a HashMap<schema::Identifier, schema::property::Property>, key: &[schema::Identifier]) -> IResult<&'a str, ast::KeyPredicate<'a>> {
	match key.len() {
		0 => panic!(),
		1 => {
			alt((
				|i| simpleKey_wip(i, ctx, props.get(&key[0]).unwrap()),
				|i| compoundKey_wip(i, ctx, props, key),
				|i| keyPathSegments_wip(i, ctx, props, key),
			))(input)
		}
		_ => {
			alt((
				|i| compoundKey_wip(i, ctx, props, key),
				|i| keyPathSegments_wip(i, ctx, props, key),
			))(input)
		}
	}
}
//* simpleKey        = OPEN ( parameterAlias / keyPropertyValue ) CLOSE
named!(simpleKey<&str, &str>, call!(recognize(tuple((OPEN, alt((parameterAlias, keyPropertyValue)), CLOSE)))));
fn simpleKey_wip<'a>(input: &'a str, ctx: &Parser, key_property: &'a schema::property::Property) -> IResult<&'a str, ast::KeyPredicate<'a>> {
	let value = alt((
		map(|i| keyPropertyValue(i), ast::KeyValue::Value),
		map(parameterAlias_wip, ast::KeyValue::ParameterAlias),
	));

	map(delimited(OPEN, value, CLOSE), |k| ast::KeyPredicate{values: vec![ast::KeyProperty{property: key_property, value: k}]})(input)
}
//* compoundKey      = OPEN keyValuePair *( COMMA keyValuePair ) CLOSE
named!(compoundKey<&str, &str>, call!(recognize(tuple((OPEN, keyValuePair, many0(tuple((COMMA, keyValuePair))), CLOSE)))));
fn compoundKey_wip<'a>(input: &'a str, ctx: &Parser, props: &'a HashMap<schema::Identifier, schema::property::Property>, key: &[schema::Identifier]) -> IResult<&'a str, ast::KeyPredicate<'a>> {
	map(delimited(OPEN, separated_nonempty_list(COMMA, |i| keyValuePair_wip(i, ctx, props, key)), CLOSE), |v| ast::KeyPredicate{values: v})(input)
}

//* keyValuePair     = ( primitiveKeyProperty / keyPropertyAlias  ) EQ ( parameterAlias / keyPropertyValue )
named!(keyValuePair<&str, &str>, call!(recognize(tuple((alt((primitiveKeyProperty, keyPropertyAlias)), EQ, alt((parameterAlias, keyPropertyValue)))))));
fn keyValuePair_wip<'a>(input: &'a str, ctx: &Parser, props: &'a HashMap<schema::Identifier, schema::property::Property>, key: &[schema::Identifier]) -> IResult<&'a str, ast::KeyProperty<'a>> {
	let (input, property) = map_opt(alt((primitiveKeyProperty, keyPropertyAlias)), |n| props.get(n))(input)?;
	let (input, value) = preceded(EQ, alt((map(parameterAlias_wip, ast::KeyValue::ParameterAlias), |i| keyPropertyValue_wip(i, ctx, property))))(input)?;

	Ok((input, ast::KeyProperty{property, value}))
}
//* keyPropertyValue = primitiveLiteral
named!(keyPropertyValue<&str, &str>, call!(recognize(primitiveLiteral)));
fn keyPropertyValue_wip<'a>(input: &'a str, ctx: &Parser, property: &'a schema::property::Property) -> IResult<&'a str, ast::KeyValue<'a>> {
	map(primitiveLiteral, ast::KeyValue::Value)(input)
}
//* keyPropertyAlias = odataIdentifier
named!(keyPropertyAlias<&str, &str>, call!(recognize(odataIdentifier)));
//* keyPathSegments  = 1*( "/" keyPathLiteral )
named!(keyPathSegments<&str, &str>, call!(recognize(many1(tuple((tag("/"), keyPathLiteral))))));
fn keyPathSegments_wip<'a>(input: &'a str, ctx: &Parser, props: &'a HashMap<schema::Identifier, schema::property::Property>, key: &[schema::Identifier]) -> IResult<&'a str, ast::KeyPredicate<'a>> {
	let mut result = vec![];

	for name in key {
		let (input, value) = preceded(tag("/"), |i| keyPathLiteral_wip(i, ctx, props.get(name).unwrap()))(input)?;
		result.push(value);
	}

	Ok((input, ast::KeyPredicate{values: result}))
}
//* keyPathLiteral   = *pchar
// FIXME This rule is overly generic. It should be matching the primitive value that the particular
// property has
named!(keyPathLiteral<&str, &str>, call!(recognize(many0(pchar))));
fn keyPathLiteral_wip<'a>(input: &'a str, ctx: &Parser, property: &'a schema::property::Property) -> IResult<&'a str, ast::KeyProperty<'a>> {
	map(recognize(many0(pchar)), |n| ast::KeyProperty{property, value: ast::KeyValue::Value(n)})(input)
}
//*
//* singleNavigation = [ "/" qualifiedEntityTypeName ]
//*                    [ "/" propertyPath
//*                    / boundOperation
//*                    / ref
//*                    / value  ; request the media resource of a media entity
//*                    ]
named!(singleNavigation<&str, &str>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(alt((recognize(tuple((tag("/"), propertyPath)))
														   , boundOperation
														   , _ref
														   , _value
														   ))))))));
fn singleNavigation_wip<'a>(input: &'a str, ctx: &'a Parser, kind: &'a schema::kind::Entity) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	//FIXME
	let (input, cast) = opt(value(ast::PathSegment::Cast, preceded(tag("/"), qualifiedEntityTypeName)))(input)?;
	let (input, path) = opt(alt((
		preceded(tag("/"), |i| propertyPath_wip(i, ctx, &kind.properties)),
		|i| boundOperation_wip(i, ctx),
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
named!(propertyPath<&str, &str>, call!(recognize(alt((tuple((entityColNavigationProperty, opt(collectionNavigation)))
						 , tuple((entityNavigationProperty, opt(singleNavigation)))
						 , tuple((complexColProperty, opt(complexColPath)))
						 , tuple((complexProperty, opt(complexPath)))
						 , tuple((primitiveColProperty, opt(primitiveColPath)))
						 , tuple((primitiveProperty, opt(primitivePath)))
						 , tuple((streamProperty, opt(boundOperation)))
						 )))));
fn propertyPath_wip<'a>(input: &'a str, ctx: &'a Parser, properties: &'a HashMap<schema::Identifier, schema::property::Property>) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	use schema::kind;
	use schema::property::*;

	let (input, property) = map_opt(odataIdentifier, |name| properties.get(name))(input)?;

	let (input, path) = match property {
		Property::Navigation(Navigation{kind, collection: true, ..}) => opt(|i| collectionNavigation_wip(i, ctx, kind))(input)?,
		Property::Navigation(Navigation{kind, collection: false, ..}) => opt(|i| singleNavigation_wip(i, ctx, kind))(input)?,
		Property::Structural(Structural{kind: Type::Complex(kind), collection: true, ..}) => opt(|i| complexColPath_wip(i, ctx, kind))(input)?,
		Property::Structural(Structural{kind: Type::Complex(kind), collection: false, ..}) => opt(|i| complexPath_wip(i, ctx, kind))(input)?,
		Property::Structural(Structural{kind: Type::Primitive(kind::Primitive::Stream), ..}) => opt(|i| boundOperation_wip(i, ctx))(input)?,
		Property::Structural(Structural{kind: Type::Primitive(_), collection: true, ..}) => opt(|i| primitiveColPath_wip(i, ctx))(input)?,
		Property::Structural(Structural{kind: Type::Primitive(_), collection: false, ..}) => opt(|i| primitivePath_wip(i, ctx))(input)?,
		Property::Structural(Structural{kind: Type::Enumeration(_), collection: true, ..}) => opt(|i| primitiveColPath_wip(i, ctx))(input)?,
		Property::Structural(Structural{kind: Type::Enumeration(_), collection: false, ..}) => opt(|i| primitivePath_wip(i, ctx))(input)?,
	};

	let mut result = vec![ast::PathSegment::Property(property)];
	if let Some(mut path) = path {
		result.append(&mut path);
	}

	Ok((input, result))
}

//*
//* primitiveColPath = count / boundOperation / ordinalIndex
named!(primitiveColPath<&str, &str>, call!(alt((count, boundOperation, ordinalIndex))));
fn primitiveColPath_wip<'a>(input: &'a str, ctx: &'a Parser) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	alt((
		map(count_wip, |c| vec![c]),
		|i| boundOperation_wip(i, ctx),
		map(|i| ordinalIndex_wip(i, ctx), |index| vec![index]),
	))(input)
}

//*
//* primitivePath  = value / boundOperation
named!(primitivePath<&str, &str>, call!(alt((_value, boundOperation))));
fn primitivePath_wip<'a>(input: &'a str, ctx: &'a Parser) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	alt((
		map(_value_wip, |c| vec![c]),
		|i| boundOperation_wip(i, ctx)
	))(input)
}
//*
//* complexColPath = ordinalIndex
//*                / [ "/" qualifiedComplexTypeName ] [ count / boundOperation ]
//  errata: The ABNF doesn't allow selecting a specific element and then continuing with a complexPath
//  rule. Is this a mistake?
named!(complexColPath<&str, &str>, call!(alt((ordinalIndex, recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(alt((count, boundOperation))))))))));
fn complexColPath_wip<'a>(input: &'a str, ctx: &'a Parser, kind: &'a schema::kind::Complex) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	alt((
		map(|i| ordinalIndex_wip(i, ctx), |index| vec![index]),
		|input| {
			let (input, cast) = opt(value(ast::PathSegment::Cast, preceded(tag("/"), qualifiedComplexTypeName)))(input)?;
			let (input, path) = opt(alt((map(count_wip, |s| vec![s]), |i| boundOperation_wip(i, ctx))))(input)?;

			let mut result = vec![];
			if let Some(cast) = cast {
				result.push(cast);
			}
			if let Some(mut path) = path {
				result.append(&mut path);
			}
			Ok((input, result))
		}
	))(input)
}
//*
//* complexPath    = [ "/" qualifiedComplexTypeName ]
//*                  [ "/" propertyPath
//*                  / boundOperation
//*                  ]
named!(complexPath<&str, &str>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(alt((recognize(tuple((tag("/"), propertyPath))), boundOperation))))))));
fn complexPath_wip<'a>(input: &'a str, ctx: &'a Parser, kind: &'a schema::kind::Complex) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	let (input, cast) = opt(value(ast::PathSegment::Cast, preceded(tag("/"), qualifiedComplexTypeName)))(input)?;
	let (input, path) = opt(alt((
		preceded(tag("/"), |i| propertyPath_wip(i, ctx, &kind.properties)),
		|i| boundOperation_wip(i, ctx),
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
named!(filterInPath<&str, &str>, call!(recognize(tuple((tag("/$filter"), EQ, parameterAlias)))));
named!(filterInPath_wip<&str, ast::PathSegment>, call!(map(preceded(tuple((tag("/$filter"), EQ)), parameterAlias_wip), |p| ast::PathSegment::Filter(p))));
//*
//* each  = '/$each'
named!(each<&str, &str>, call!(tag("/$each")));
named!(each_wip<&str, ast::PathSegment>, call!(value(ast::PathSegment::Each, tag("/$each"))));
//* count = '/$count'
named!(count<&str, &str>, call!(tag("/$count")));
named!(count_wip<&str, ast::PathSegment>, call!(value(ast::PathSegment::Count, tag("/$count"))));
//* ref   = '/$ref'
named!(_ref<&str, &str>, call!(tag("/$ref")));
named!(_ref_wip<&str, ast::PathSegment>, call!(value(ast::PathSegment::Ref, tag("/$ref"))));
//* value = '/$value'
named!(_value<&str, &str>, call!(tag("/$value")));
named!(_value_wip<&str, ast::PathSegment>, call!(value(ast::PathSegment::Value, tag("/$value"))));
//*
//* ordinalIndex = "/" 1*DIGIT
//  errata: Even though the ABNF encodes only positive integers, the OData spec defines negative ordinal
//  indices too. See:
//  http://docs.oasis-open.org/odata/odata/v4.01/cs01/part1-protocol/odata-v4.01-cs01-part1-protocol.html#sec_RequestinganIndividualMemberofanOrde
named!(ordinalIndex<&str, &str>, call!(recognize(tuple((tag("/"), many1(DIGIT))))));
fn ordinalIndex_wip<'a>(input: &'a str, ctx: &'a Parser) -> IResult<&'a str, ast::PathSegment<'a>> {
	map(preceded(tag("/"), map_res(recognize(tuple((opt(tag("-")), digit1))), |n: &str| n.parse())), ast::PathSegment::OrdinalIndex)(input)
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
named!(boundOperation<&str, &str>, call!(recognize(tuple((tag("/"), alt((boundActionCall
								     , recognize(tuple((boundEntityColFunctionCall, opt(collectionNavigation))))
								     , recognize(tuple((boundEntityFunctionCall, opt(singleNavigation))))
								     , recognize(tuple((boundComplexColFunctionCall, opt(complexColPath))))
								     , recognize(tuple((boundComplexFunctionCall, opt(complexPath))))
								     , recognize(tuple((boundPrimitiveColFunctionCall, opt(primitiveColPath))))
								     , recognize(tuple((boundPrimitiveFunctionCall, opt(primitivePath))))
								     , boundFunctionCallNoParens
								     )))))));
fn boundOperation_wip<'a>(input: &'a str, ctx: &'a Parser) -> IResult<&'a str, Vec<ast::PathSegment<'a>>> {
	// FIXME
	value(vec![], boundOperation)(input)
}
//*
//* actionImportCall = actionImport
named!(actionImportCall<&str, &str>, call!(recognize(actionImport)));
//* boundActionCall  = namespace "." action
//*                    ; with the added restriction that the binding parameter MUST be either an entity or collection of entities
//*                    ; and is specified by reference using the URI immediately preceding (to the left) of the boundActionCall
named!(boundActionCall<&str, &str>, call!(recognize(tuple((namespace, tag("."), action)))));
//*
//* ; The following boundXxxFunctionCall rules have the added restrictions that
//* ;  - the function MUST support binding, and
//* ;  - the binding parameter type MUST match the type of resource identified by the
//* ;    URI immediately preceding (to the left) of the boundXxxFunctionCall, and
//* ;  - the functionParameters MUST NOT include the bindingParameter.
//TODO(validate)
//* boundEntityFunctionCall       = namespace "." entityFunction       functionParameters
named!(boundEntityFunctionCall<&str, &str>, call!(recognize(tuple((entityFunction, functionParameters)))));
//* boundEntityColFunctionCall    = namespace "." entityColFunction    functionParameters
named!(boundEntityColFunctionCall<&str, &str>, call!(recognize(tuple((entityColFunction, functionParameters)))));
//* boundComplexFunctionCall      = namespace "." complexFunction      functionParameters
named!(boundComplexFunctionCall<&str, &str>, call!(recognize(tuple((complexFunction, functionParameters)))));
//* boundComplexColFunctionCall   = namespace "." complexColFunction   functionParameters
named!(boundComplexColFunctionCall<&str, &str>, call!(recognize(tuple((complexColFunction, functionParameters)))));
//* boundPrimitiveFunctionCall    = namespace "." primitiveFunction    functionParameters
named!(boundPrimitiveFunctionCall<&str, &str>, call!(recognize(tuple((primitiveFunction, functionParameters)))));
//* boundPrimitiveColFunctionCall = namespace "." primitiveColFunction functionParameters
named!(boundPrimitiveColFunctionCall<&str, &str>, call!(recognize(tuple((primitiveColFunction, functionParameters)))));
//*
//* boundFunctionCallNoParens     = namespace "." entityFunction
//*                               / namespace "." entityColFunction
//*                               / namespace "." complexFunction
//*                               / namespace "." complexColFunction
//*                               / namespace "." primitiveFunction
//*                               / namespace "." primitiveColFunction
named!(boundFunctionCallNoParens<&str, &str>, call!(recognize(alt((tuple((namespace, tag("."), entityFunction))
							      , tuple((namespace, tag("."), entityColFunction))
						   	      , tuple((namespace, tag("."), complexFunction))
						   	      , tuple((namespace, tag("."), complexColFunction))
						   	      , tuple((namespace, tag("."), primitiveFunction))
						   	      , tuple((namespace, tag("."), primitiveColFunction))
							 )))));

//*
//* entityFunctionImportCall       = entityFunctionImport       functionParameters
named!(entityFunctionImportCall<&str, &str>, call!(recognize(tuple((entityFunctionImport,functionParameters)))));
//* entityColFunctionImportCall    = entityColFunctionImport    functionParameters
named!(entityColFunctionImportCall<&str, &str>, call!(recognize(tuple((entityColFunctionImport,functionParameters)))));
//* complexFunctionImportCall      = complexFunctionImport      functionParameters
named!(complexFunctionImportCall<&str, &str>, call!(recognize(tuple((complexFunctionImport,functionParameters)))));
//* complexColFunctionImportCall   = complexColFunctionImport   functionParameters
named!(complexColFunctionImportCall<&str, &str>, call!(recognize(tuple((complexColFunctionImport,functionParameters)))));
//* primitiveFunctionImportCall    = primitiveFunctionImport    functionParameters
named!(primitiveFunctionImportCall<&str, &str>, call!(recognize(tuple((primitiveFunctionImport,functionParameters)))));
//* primitiveColFunctionImportCall = primitiveColFunctionImport functionParameters
named!(primitiveColFunctionImportCall<&str, &str>, call!(recognize(tuple((primitiveColFunctionImport,functionParameters)))));
//*
//* functionImportCallNoParens     = entityFunctionImport
//*                                / entityColFunctionImport
//*                                / complexFunctionImport
//*                                / complexColFunctionImport
//*                                / primitiveFunctionImport
//*                                / primitiveColFunctionImport
named!(functionImportCallNoParens<&str, &str>, call!(alt((entityFunctionImport
						    , entityColFunctionImport
						    , complexFunctionImport
						    , complexColFunctionImport
						    , primitiveFunctionImport
						    , primitiveColFunctionImport))));
//*
//* functionParameters = OPEN [ functionParameter *( COMMA functionParameter ) ] CLOSE
named!(functionParameters<&str, &str>, call!(recognize(tuple((OPEN, opt(tuple((functionParameter, many0(tuple((COMMA, functionParameter)))))), CLOSE)))));
//* functionParameter  = parameterName EQ ( parameterAlias / primitiveLiteral )
named!(functionParameter<&str, &str>, call!(recognize(tuple((parameterName, EQ, alt((parameterAlias, primitiveLiteral)))))));
//* parameterName      = odataIdentifier
named!(parameterName<&str, &str>, call!(recognize(odataIdentifier)));
//* parameterAlias     = AT odataIdentifier
named!(parameterAlias<&str, &str>, call!(preceded(AT, odataIdentifier)));
named!(parameterAlias_wip<&str, ast::ParameterAlias>, call!(map(preceded(AT, odataIdentifier), |name| ast::ParameterAlias{name})));
//*
//* crossjoin = '$crossjoin' OPEN
//*             entitySetName *( COMMA entitySetName )
//*             CLOSE
named!(crossjoin<&str, &str>, call!(recognize(tuple((tag("$crossjoin"), OPEN, entitySetName, many0(tuple((COMMA, entitySetName))), CLOSE)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 2. Query Options
//* ;------------------------------------------------------------------------------
//*
//* queryOptions = queryOption *( "&" queryOption )
named!(queryOptions<&str, &str>, call!(recognize(tuple((queryOption, many0(tuple((tag("&"), queryOption))))))));
fn queryOptions_wip<'a>(input: &'a str, ctx: &Parser) -> IResult<&'a str, Vec<ast::QueryOption<'a>>> {
	separated_nonempty_list(tag("&"), |i| queryOption_wip(i, ctx))(input)
}
//* queryOption  = systemQueryOption
//*              / aliasAndValue
//*              / nameAndValue
//*              / customQueryOption
named!(queryOption<&str, &str>, call!(alt((systemQueryOption, aliasAndValue, nameAndValue, customQueryOption))));
fn queryOption_wip<'a>(input: &'a str, ctx: &Parser) -> IResult<&'a str, ast::QueryOption<'a>> {
	alt((
		|i| systemQueryOption_wip(i, ctx),
		value(ast::QueryOption::Alias, aliasAndValue),
		value(ast::QueryOption::Name, nameAndValue),
		customQueryOption_wip,
	))(input)
}
//*
//* batchOptions = batchOption *( "&" batchOption )
named!(batchOptions<&str, Vec<ast::QueryOption>>, call!(separated_nonempty_list(tag("&"), batchOption)));
//* batchOption  = format
//*              /customQueryOption
named!(batchOption<&str, ast::QueryOption>, call!(alt((format_wip, customQueryOption_wip))));
//*
//* metadataOptions = metadataOption *( "&" metadataOption )
named!(metadataOptions<&str, &str>, call!(recognize(tuple((metadataOption, many0(tuple((tag("&"), metadataOption))))))));
//* metadataOption  = format
//*                 /customQueryOption
named!(metadataOption<&str, &str>, call!(alt((format, customQueryOption))));
//*
//* entityOptions  = *( entityIdOption "&" ) id *( "&" entityIdOption )
named!(entityOptions<&str, &str>, call!(recognize(tuple((many0(tuple((entityIdOption, tag("&")))), id, many0(tuple((tag("&"), entityIdOption))))))));
//* entityIdOption = format
//*                / customQueryOption
named!(entityIdOption<&str, &str>, call!(alt((format, customQueryOption))));
//* entityCastOptions = *( entityCastOption "&" ) id *( "&" entityCastOption )
named!(entityCastOptions<&str, &str>, call!(recognize(tuple((many0(tuple((entityCastOption, tag("&")))), id, many0(tuple((tag("&"), entityCastOption))))))));
//* entityCastOption  = entityIdOption
//*                   / expand
//*                   / select
named!(entityCastOption<&str, &str>, call!(alt((entityIdOption, expand, select))));
//*
//* id = ( "$id" / "id" ) EQ IRI-in-query
named!(id<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$id"), tag_no_case("id"))), EQ, IRI_in_query)))));
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
named!(systemQueryOption<&str, &str>, call!(alt((compute
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
fn systemQueryOption_wip<'a>(input: &'a str, ctx: &Parser) -> IResult<&'a str, ast::QueryOption<'a>> {
	alt((
		value(ast::QueryOption::Compute, compute),
		value(ast::QueryOption::DeltaToken, deltatoken),
		value(ast::QueryOption::Expand, expand),
		|i| filter_wip(i, ctx),
		format_wip,
		value(ast::QueryOption::Id, id),
		value(ast::QueryOption::InlineCount, inlinecount),
		value(ast::QueryOption::OrderBy, orderby),
		value(ast::QueryOption::SchemaVersion, schemaversion),
		value(ast::QueryOption::Search, search),
		value(ast::QueryOption::Select, select),
		value(ast::QueryOption::Skip, skip),
		value(ast::QueryOption::SkipToken, skiptoken),
		value(ast::QueryOption::Top, top),
		value(ast::QueryOption::Index, index),
	))(input)
}

//*
//* compute          = ( "$compute" / "compute" ) EQ computeItem *( COMMA computeItem )
named!(compute<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$compute"), tag_no_case("compute"))), EQ, computeItem, many0(tuple((COMMA, computeItem))))))));
//* computeItem      = commonExpr RWS "as" RWS computedProperty
named!(computeItem<&str, &str>, call!(recognize(tuple((commonExpr, RWS, tag_no_case("as"), RWS, computedProperty)))));
//* computedProperty = odataIdentifier
named!(computedProperty<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* expand            = ( "$expand" / "expand" ) EQ expandItem *( COMMA expandItem )
named!(expand<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$expand"), tag_no_case("expand"))), EQ, expandItem, many0(tuple((COMMA, expandItem))))))));
//* expandItem        = STAR [ ref / OPEN levels CLOSE ]
//*                   / "$value"
//*                   / expandPath
//*                     [ ref   [ OPEN expandRefOption   *( SEMI expandRefOption   ) CLOSE ]
//*                     / count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                     /         OPEN expandOption      *( SEMI expandOption      ) CLOSE
//*                     ]
named!(expandItem<&str, &str>, call!(recognize(alt((recognize(tuple((STAR, opt(alt((_ref, recognize(tuple((OPEN, levels, CLOSE)))))))))
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
named!(expandPath<&str, &str>, call!(recognize(tuple((
						opt(tuple((alt((qualifiedEntityTypeName, qualifiedComplexTypeName)), tag("/")))),
						many0(tuple((alt((complexProperty, complexColProperty)), tag("/"), opt(tuple((qualifiedComplexTypeName, tag("/"))))))),
						alt((STAR, streamProperty, recognize(tuple((navigationProperty, opt(tuple((tag("/"), qualifiedEntityTypeName))))))))
						)))));

//* expandCountOption = filter
//*                   / search
named!(expandCountOption<&str, &str>, call!(alt((filter, search))));
//* expandRefOption   = expandCountOption
//*                   / orderby
//*                   / skip
//*                   / top
//*                   / inlinecount
named!(expandRefOption<&str, &str>, call!(alt((expandCountOption, orderby, skip, top, inlinecount))));
//* expandOption      = expandRefOption
//*                   / select
//*                   / expand
//*                   / compute
//*                   / levels
//*                   / aliasAndValue
named!(expandOption<&str, &str>, call!(alt((expandRefOption, select, expand, compute, levels, aliasAndValue))));
//*
//* levels = ( "$levels" / "levels" ) EQ ( oneToNine *DIGIT / "max" )
named!(levels<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$levels"), tag_no_case("levels"))), EQ, alt((recognize(tuple((oneToNine, many0(DIGIT)))), tag_no_case("max"))))))));
//*
//* filter = ( "$filter" / "filter" ) EQ boolCommonExpr
named!(filter<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$filter"), tag_no_case("filter"))), EQ, boolCommonExpr)))));
fn filter_wip<'a>(input: &'a str, ctx: &Parser) -> IResult<&'a str, ast::QueryOption<'a>> {
	println!("parsing filter {}", input);
	map(preceded(tuple((opt(tag("$")), tag_no_case("filter"), EQ)), boolCommonExpr), ast::QueryOption::Filter)(input)
}
//*
//* orderby     = ( "$orderby" / "orderby" ) EQ orderbyItem *( COMMA orderbyItem )
named!(orderby<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$orderby"), tag_no_case("orderby"))), EQ, orderbyItem, many0(tuple((COMMA, orderbyItem))))))));
//* orderbyItem = commonExpr [ RWS ( "asc" / "desc" ) ]
named!(orderbyItem<&str, &str>, call!(recognize(tuple((commonExpr, opt(tuple((RWS, alt((tag_no_case("asc"), tag_no_case("desc")))))))))));
//*
//* skip = ( "$skip" / "skip" ) EQ 1*DIGIT
named!(skip<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$skip"), tag_no_case("skip"))), EQ, many1(DIGIT))))));
//* top  = ( "$top"  / "top"  ) EQ 1*DIGIT
named!(top<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$top"), tag_no_case("top"))), EQ, many1(DIGIT))))));
//*
//* index  = ( "$index" / "index" ) EQ 1*DIGIT
named!(index<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$index"), tag_no_case("index"))), EQ, many1(DIGIT))))));
//*
//* format = ( "$format" / "format" ) EQ
//*          ( "atom"
//*          / "json"
//*          / "xml"
//*          / 1*pchar "/" 1*pchar ; <a data service specific value indicating a
//*          )                     ; format specific to the specific data service> or
//*                                ; <An IANA-defined [IANA-MMT] content type>
// errata: pchar includes special characters like & and =. It should probably be something like qchar_no_AMP_EQ
named!(format_wip<&str, ast::QueryOption>, call!(preceded(
					     tuple((opt(tag("$")), tag_no_case("format"), EQ)),
					     map(
						alt((
							     nom::combinator::value(ast::FormatKind::Atom, tag_no_case("atom"))
							   , nom::combinator::value(ast::FormatKind::JSON, tag_no_case("json"))
							   , nom::combinator::value(ast::FormatKind::XML, tag_no_case("xml"))
							   , map(recognize(tuple((many1(pchar), tag("/"), many1(pchar)))), ast::FormatKind::Custom)
						)),
						ast::QueryOption::Format
					     ))));
named!(format<&str, &str>, call!(recognize(format_wip)));

//*
//* inlinecount = ( "$count" / "count" ) EQ booleanValue
named!(inlinecount<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$count"), tag_no_case("count"))), EQ, booleanValue)))));
//*
//* schemaversion   = ( "$schemaversion" / "schemaversion" ) EQ ( STAR / 1*unreserved )
named!(schemaversion<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$schemaversion"), tag_no_case("schemaversion"))), EQ, alt((STAR, recognize(many1(unreserved)))))))));
//*
//* search     = ( "$search" / "search" ) EQ BWS searchExpr
named!(search<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$search"), tag_no_case("search"))), EQ, BWS, searchExpr)))));
//* searchExpr = ( OPEN BWS searchExpr BWS CLOSE
//*              / searchTerm
//*              ) [ searchOrExpr
//*                / searchAndExpr
//*                ]
named!(searchExpr<&str, &str>, call!(recognize(tuple((alt((recognize(tuple((OPEN, BWS, searchExpr, BWS, CLOSE))), searchTerm)), opt(alt((searchOrExpr, searchAndExpr))))))));
//*
//* searchOrExpr  = RWS 'OR'  RWS searchExpr
named!(searchOrExpr<&str, &str>, call!(recognize(tuple((RWS, tag("OR"), RWS, searchExpr)))));
//* searchAndExpr = RWS [ 'AND' RWS ] searchExpr
named!(searchAndExpr<&str, &str>, call!(recognize(tuple((RWS, opt(tuple((tag("AND"), RWS))), searchExpr)))));
//*
//* searchTerm   = [ 'NOT' RWS ] ( searchPhrase / searchWord )
named!(searchTerm<&str, &str>, call!(recognize(tuple((opt(tuple((tag("NOT"), RWS))), alt((searchPhrase, searchWord)))))));
//* searchPhrase = quotation-mark 1*qchar-no-AMP-DQUOTE quotation-mark
named!(searchPhrase<&str, &str>, call!(recognize(tuple((quotation_mark, many1(qchar_no_AMP_DQUOTE), quotation_mark)))));
//*
//* ; A searchWord is a sequence of one or more letters, digits, commas, or dots.
//* ; This includes Unicode characters of categories L or N using UTF-8 and percent-encoding.
//* ; The words AND, OR, and NOT are not a valid searchWord.
//* ; Expressing this in ABNF is somewhat clumsy, so the following rule is overly generous.
//TODO(validation)
//* searchWord   = 1*( ALPHA / DIGIT / COMMA / "." / pct-encoded )
named!(searchWord<&str, &str>, call!(recognize(many1(alt((ALPHA, DIGIT, COMMA, tag("."), pct_encoded))))));
//*
//* select         = ( "$select" / "select" ) EQ selectItem *( COMMA selectItem )
named!(select<&str, &str>, call!(recognize(tuple((alt((tag_no_case("$select"), tag_no_case("select"))), EQ, selectItem, many0(tuple((COMMA, selectItem))))))));
//* selectItem     = STAR
//*                / allOperationsInSchema
//*                / [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                  ( selectProperty
//*                  / qualifiedActionName
//*                  / qualifiedFunctionName
//*                  )
named!(selectItem<&str, &str>, call!(alt((STAR
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
named!(selectProperty<&str, &str>, call!(alt((primitiveProperty
					, recognize(tuple((primitiveColProperty, opt(tuple((OPEN, selectOptionPC, many0(tuple((SEMI, selectOptionPC))), CLOSE))))))
					, navigationProperty
					, recognize(tuple((selectPath, opt(alt((
										recognize(tuple((OPEN, selectOption, many0(tuple((SEMI, selectOption))), CLOSE)))
										, recognize(tuple((tag("/"), selectProperty)))
										))))))
					))));
//* selectPath     = ( complexProperty / complexColProperty ) [ "/" qualifiedComplexTypeName ]
named!(selectPath<&str, &str>, call!(recognize(tuple((alt((complexProperty, complexColProperty)), opt(tuple((tag("/"), qualifiedComplexTypeName))))))));
//* selectOptionPC = filter / search / inlinecount / orderby / skip / top
named!(selectOptionPC<&str, &str>, call!(alt((filter, search, inlinecount, orderby, skip, top))));
//* selectOption   = selectOptionPC
//*                / compute / select / expand / aliasAndValue
named!(selectOption<&str, &str>, call!(alt((selectOptionPC, compute, select, expand, aliasAndValue))));
//*
//* allOperationsInSchema = namespace "." STAR
named!(allOperationsInSchema<&str, &str>, call!(recognize(tuple((namespace, tag("."), STAR)))));
//*
//* ; The parameterNames uniquely identify the bound function overload
//* ; only if it has overloads.
//* qualifiedActionName   = namespace "." action
named!(qualifiedActionName<&str, &str>, call!(recognize(tuple((namespace, tag("."), action)))));
//* qualifiedFunctionName = namespace "." function [ OPEN parameterNames CLOSE ]
named!(qualifiedFunctionName<&str, &str>, call!(recognize(tuple((namespace, tag("."), function, opt(tuple((OPEN, parameterNames, CLOSE))))))));
//*
//* ; The names of all non-binding parameters, separated by commas
//* parameterNames = parameterName *( COMMA parameterName )
named!(parameterNames<&str, &str>, call!(recognize(tuple((parameterName, many0(tuple((COMMA, parameterName))))))));
//*
//* deltatoken = "$deltatoken" EQ 1*( qchar-no-AMP )
named!(deltatoken<&str, &str>, call!(recognize(tuple((tag_no_case("$deltatoken"), EQ, many1(qchar_no_AMP))))));
//*
//* skiptoken = "$skiptoken" EQ 1*( qchar-no-AMP )
named!(skiptoken<&str, &str>, call!(recognize(tuple((tag_no_case("$skiptoken"), EQ, many1(qchar_no_AMP))))));
//*
//* aliasAndValue = parameterAlias EQ parameterValue
named!(aliasAndValue<&str, &str>, call!(recognize(tuple((parameterAlias, EQ, parameterValue)))));
//*
//* nameAndValue = parameterName EQ parameterValue
named!(nameAndValue<&str, &str>, call!(recognize(tuple((parameterName, EQ, parameterValue)))));
//*
//* parameterValue = arrayOrObject
//*                / commonExpr
named!(parameterValue<&str, &str>, call!(alt((arrayOrObject, commonExpr))));
//*
//* customQueryOption = customName [ EQ customValue ]
named!(customQueryOption_wip<&str, ast::QueryOption>, do_parse!(foo: call!(recognize(tuple((customName, opt(tuple((EQ, customValue))))))) >> (ast::QueryOption::Custom(foo))));
named!(customQueryOption<&str, &str>, call!(recognize(customQueryOption_wip)));
//* customName        = qchar-no-AMP-EQ-AT-DOLLAR *( qchar-no-AMP-EQ )
named!(customName<&str, &str>, call!(recognize(tuple((qchar_no_AMP_EQ_AT_DOLLAR, many0(qchar_no_AMP_EQ))))));
//* customValue       = *( qchar-no-AMP )
named!(customValue<&str, &str>, call!(recognize(many0(qchar_no_AMP))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 3. Context URL Fragments
//* ;------------------------------------------------------------------------------
//*
//* context         = "#" contextFragment
named!(context<&str, &str>, call!(recognize(tuple((tag("#"), contextFragment)))));
//* contextFragment = 'Collection($ref)'
//*                 / '$ref'
//*                 / 'Collection(Edm.EntityType)'
//*                 / 'Collection(Edm.ComplexType)'
//*                 / singletonEntity [ navigation *( containmentNavigation ) [ "/" qualifiedEntityTypeName ] ] [ selectList ]
//*                 / qualifiedTypeName [ selectList ]
//*                 / entitySet ( '/$deletedEntity' / '/$link' / '/$deletedLink' )
//*                 / entitySet keyPredicate "/" contextPropertyPath [ selectList ]
//*                 / entitySet [ selectList ] [ '/$entity' / '/$delta' ]
named!(contextFragment<&str, &str>, call!(alt((tag("Collection($ref)")
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
named!(entitySet<&str, &str>, call!(recognize(tuple((entitySetName, many0(containmentNavigation), opt(tuple((tag("/"), qualifiedEntityTypeName))))))));
//*
//* containmentNavigation = keyPredicate [ "/" qualifiedEntityTypeName ] navigation
named!(containmentNavigation<&str, &str>, call!(recognize(tuple((keyPredicate, opt(tuple((tag("/"), qualifiedEntityTypeName))), navigation)))));
//* navigation            = *( "/" complexProperty [ "/" qualifiedComplexTypeName ] ) "/" navigationProperty
named!(navigation<&str, &str>, call!(recognize(tuple((many0(tuple((tag("/"), complexProperty, opt(tuple((tag("/"), qualifiedComplexTypeName)))))), tag("/"), navigationProperty)))));
//*
//* selectList         = OPEN selectListItem *( COMMA selectListItem ) CLOSE
named!(selectList<&str, &str>, call!(recognize(tuple((OPEN, selectListItem, many0(tuple((COMMA, selectListItem))), CLOSE)))));
//* selectListItem     = STAR ; all structural properties
//*                    / allOperationsInSchema
//*                    / [ qualifiedEntityTypeName "/" ]
//*                      ( qualifiedActionName
//*                      / qualifiedFunctionName
//*                      / selectListProperty
//*                      )
named!(selectListItem<&str, &str>, call!(alt((STAR
					, allOperationsInSchema
					, recognize(tuple((opt(tuple((qualifiedEntityTypeName, tag("/")))), alt((qualifiedActionName, qualifiedFunctionName, selectListProperty)))))
				       ))));
//* selectListProperty = primitiveProperty
//*                    / primitiveColProperty
//*                    / navigationProperty [ "+" ] [ selectList ]
//*                    / selectPath [ "/" selectListProperty ]
named!(selectListProperty<&str, &str>, call!(alt((primitiveProperty
					    , primitiveColProperty
					    , recognize(tuple((navigationProperty, opt(tag("+")), opt(selectList))))
					    , recognize(tuple((selectPath, opt(tuple((tag("/"), selectListProperty))))))
					   ))));
//*
//* contextPropertyPath = primitiveProperty
//*                     / primitiveColProperty
//*                     / complexColProperty
//*                     / complexProperty [ [ "/" qualifiedComplexTypeName ] "/" contextPropertyPath ]
named!(contextPropertyPath<&str, &str>, call!(alt((primitiveProperty
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
named!(commonExpr<&str, &str>, call!(recognize(tuple((
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
//*
//* boolCommonExpr = commonExpr ; resulting in a Boolean
//TODO(validate)
named!(boolCommonExpr<&str, &str>, call!(recognize(commonExpr)));
//*
//* rootExpr = '$root/' ( entitySetName keyPredicate / singletonEntity ) [ singleNavigationExpr ]
named!(rootExpr<&str, &str>, call!(recognize(tuple((tag("$root/"), alt((recognize(tuple((entitySetName, keyPredicate))), singletonEntity)), opt(singleNavigationExpr))))));
//*
//* firstMemberExpr = memberExpr
//*                 / inscopeVariableExpr [ "/" memberExpr ]
named!(firstMemberExpr<&str, &str>, call!(alt((memberExpr, recognize(tuple((inscopeVariableExpr, opt(tuple((tag("/"), memberExpr))))))))));
//*
//* memberExpr = [ qualifiedEntityTypeName "/" ]
//*              ( propertyPathExpr
//*              / boundFunctionExpr
//*              / annotationExpr
//*              )
named!(memberExpr<&str, &str>, call!(recognize(tuple((opt(tuple((qualifiedEntityTypeName, tag("/")))), alt((propertyPathExpr, boundFunctionExpr, annotationExpr)))))));
//*
//* propertyPathExpr = ( entityColNavigationProperty [ collectionNavigationExpr ]
//*                    / entityNavigationProperty    [ singleNavigationExpr ]
//*                    / complexColProperty          [ complexColPathExpr ]
//*                    / complexProperty             [ complexPathExpr ]
//*                    / primitiveColProperty        [ collectionPathExpr ]
//*                    / primitiveProperty           [ primitivePathExpr ]
//*                    / streamProperty              [ primitivePathExpr ]
//*                    )
named!(propertyPathExpr<&str, &str>, call!(recognize(alt((
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
named!(annotationExpr<&str, &str>, call!(recognize(tuple((annotation, opt(alt((collectionPathExpr, singleNavigationExpr, complexPathExpr, primitivePathExpr))))))));
//*
//* annotation          = AT [ namespace "." ] termName [ '#' annotationQualifier ]
named!(annotation<&str, &str>, call!(recognize(tuple((AT, opt(tuple((namespace, tag(".")))), termName, opt(tuple((tag("#"), annotationQualifier))))))));
//* annotationQualifier = odataIdentifier
named!(annotationQualifier<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* inscopeVariableExpr  = implicitVariableExpr
//*                      / parameterAlias
//*                      / lambdaVariableExpr ; only allowed inside a lambdaPredicateExpr
//TODO(validation)
named!(inscopeVariableExpr<&str, &str>, call!(alt((implicitVariableExpr, parameterAlias, lambdaVariableExpr))));
//* implicitVariableExpr = '$it'              ; the current instance of the resource identified by the resource path
//*                      / '$this'            ; the instance on which the query option is evaluated
named!(implicitVariableExpr<&str, &str>, call!(alt((tag("$it"), tag("$this")))));
//* lambdaVariableExpr   = odataIdentifier
named!(lambdaVariableExpr<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* collectionNavigationExpr = [ "/" qualifiedEntityTypeName ]
//*                            [ keyPredicate [ singleNavigationExpr ]
//*                            / collectionPathExpr
//*                            ]
named!(collectionNavigationExpr<&str, &str>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedEntityTypeName))), opt(alt((
															   recognize(tuple((keyPredicate, opt(singleNavigationExpr))))
															   , collectionPathExpr
															))))))));
//*
//* singleNavigationExpr = "/" memberExpr
named!(singleNavigationExpr<&str, &str>, call!(recognize(tuple((tag("/"), memberExpr)))));
//*
//* complexColPathExpr = [ "/" qualifiedComplexTypeName ]
//*                      [ collectionPathExpr ]
named!(complexColPathExpr<&str, &str>, call!(recognize(tuple((opt(tuple((tag("/"), qualifiedComplexTypeName))), opt(collectionPathExpr))))));
//*
//* collectionPathExpr = count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                    / "/" boundFunctionExpr
//*                    / "/" annotationExpr
//*                    / "/" anyExpr
//*                    / "/" allExpr
named!(collectionPathExpr<&str, &str>, call!(alt((
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
named!(complexPathExpr<&str, &str>, call!(recognize(tuple((
						      opt(tuple((tag("/"), qualifiedComplexTypeName))),
						      opt(alt((
								tuple((tag("/"), propertyPathExpr))
								, tuple((tag("/"), boundFunctionExpr))
								, tuple((tag("/"), annotationExpr))
						      )))
						)))));
//*
//* primitivePathExpr = "/" [ annotationExpr / boundFunctionExpr ]
named!(primitivePathExpr<&str, &str>, call!(recognize(tuple((tag("/"), opt(alt((annotationExpr, boundFunctionExpr))))))));
//*
//* boundFunctionExpr = functionExpr ; boundFunction segments can only be composed if the type of the
//*                                  ; previous segment matches the type of the first function parameter
//TODO(validation)
named!(boundFunctionExpr<&str, &str>, call!(recognize(functionExpr)));
//*
//* functionExpr = namespace "."
//*                ( entityColFunction    functionExprParameters [ collectionNavigationExpr ]
//*                / entityFunction       functionExprParameters [ singleNavigationExpr ]
//*                / complexColFunction   functionExprParameters [ complexColPathExpr ]
//*                / complexFunction      functionExprParameters [ complexPathExpr ]
//*                / primitiveColFunction functionExprParameters [ collectionPathExpr ]
//*                / primitiveFunction    functionExprParameters [ primitivePathExpr ]
//*                )
named!(functionExpr<&str, &str>, call!(recognize(tuple((namespace,
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
named!(functionExprParameters<&str, &str>, call!(recognize(tuple((OPEN, opt(tuple((functionExprParameter, many0(tuple((COMMA, functionExprParameter)))))), CLOSE)))));
//* functionExprParameter  = parameterName EQ ( parameterAlias / parameterValue )
named!(functionExprParameter<&str, &str>, call!(recognize(tuple((parameterName, EQ, alt((parameterAlias, parameterValue)))))));
//*
//* anyExpr = "any" OPEN BWS [ lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr ] BWS CLOSE
named!(anyExpr<&str, &str>, call!(recognize(tuple((tag_no_case("any"), OPEN, BWS, opt(tuple((lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr))), BWS, CLOSE)))));
//* allExpr = "all" OPEN BWS   lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr   BWS CLOSE
named!(allExpr<&str, &str>, call!(recognize(tuple((tag_no_case("all"), OPEN, BWS, lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr, BWS, CLOSE)))));
//* lambdaPredicateExpr = boolCommonExpr ; containing at least one lambdaVariableExpr
//TODO(use verify() to verify that it contains at least one lambdaVariableExpr)
named!(lambdaPredicateExpr<&str, &str>, call!(recognize(boolCommonExpr)));
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
named!(methodCallExpr<&str, &str>, call!(alt((
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
//*
//* boolMethodCallExpr = endsWithMethodCallExpr
//*                    / startsWithMethodCallExpr
//*                    / containsMethodCallExpr
//*                    / intersectsMethodCallExpr
//*                    / hasSubsetMethodCallExpr
//*                    / hasSubsequenceMethodCallExpr
named!(boolMethodCallExpr<&str, &str>, call!(alt((endsWithMethodCallExpr
					    , startsWithMethodCallExpr
					    , containsMethodCallExpr
					    , intersectsMethodCallExpr
					    , hasSubsetMethodCallExpr
					    , hasSubsequenceMethodCallExpr))));
//*
//* concatMethodCallExpr     = "concat"     OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(concatMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("concat"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* containsMethodCallExpr   = "contains"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(containsMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("contains"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* endsWithMethodCallExpr   = "endswith"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(endsWithMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("endswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* indexOfMethodCallExpr    = "indexof"    OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(indexOfMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("indexof"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* lengthMethodCallExpr     = "length"     OPEN BWS commonExpr BWS CLOSE
named!(lengthMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("length"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* startsWithMethodCallExpr = "startswith" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(startsWithMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("startswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* substringMethodCallExpr  = "substring"  OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS [ COMMA BWS commonExpr BWS ] CLOSE
named!(substringMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("substring"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, opt(tuple((COMMA, BWS, commonExpr, BWS))), CLOSE)))));
//* toLowerMethodCallExpr    = "tolower"    OPEN BWS commonExpr BWS CLOSE
named!(toLowerMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("tolower"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* toUpperMethodCallExpr    = "toupper"    OPEN BWS commonExpr BWS CLOSE
named!(toUpperMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("toupper"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* trimMethodCallExpr       = "trim"       OPEN BWS commonExpr BWS CLOSE
named!(trimMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("trim"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* yearMethodCallExpr               = "year"               OPEN BWS commonExpr BWS CLOSE
named!(yearMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("year"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* monthMethodCallExpr              = "month"              OPEN BWS commonExpr BWS CLOSE
named!(monthMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("month"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* dayMethodCallExpr                = "day"                OPEN BWS commonExpr BWS CLOSE
named!(dayMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("day"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* hourMethodCallExpr               = "hour"               OPEN BWS commonExpr BWS CLOSE
named!(hourMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("hour"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* minuteMethodCallExpr             = "minute"             OPEN BWS commonExpr BWS CLOSE
named!(minuteMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("minute"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* secondMethodCallExpr             = "second"             OPEN BWS commonExpr BWS CLOSE
named!(secondMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("second"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* fractionalsecondsMethodCallExpr  = "fractionalseconds"  OPEN BWS commonExpr BWS CLOSE
named!(fractionalsecondsMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("fractionalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* totalsecondsMethodCallExpr       = "totalseconds"       OPEN BWS commonExpr BWS CLOSE
named!(totalsecondsMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("totalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* dateMethodCallExpr               = "date"               OPEN BWS commonExpr BWS CLOSE
named!(dateMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("date"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* timeMethodCallExpr               = "time"               OPEN BWS commonExpr BWS CLOSE
named!(timeMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("time"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* totalOffsetMinutesMethodCallExpr = "totaloffsetminutes" OPEN BWS commonExpr BWS CLOSE
named!(totalOffsetMinutesMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("totaloffsetminutes"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* minDateTimeMethodCallExpr = "mindatetime" OPEN BWS CLOSE
named!(minDateTimeMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("mindatetime"), OPEN, BWS, CLOSE)))));
//* maxDateTimeMethodCallExpr = "maxdatetime" OPEN BWS CLOSE
named!(maxDateTimeMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("maxdatetime"), OPEN, BWS, CLOSE)))));
//* nowMethodCallExpr         = "now"         OPEN BWS CLOSE
named!(nowMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("now"), OPEN, BWS, CLOSE)))));
//*
//* roundMethodCallExpr   = "round"   OPEN BWS commonExpr BWS CLOSE
named!(roundMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("round"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* floorMethodCallExpr   = "floor"   OPEN BWS commonExpr BWS CLOSE
named!(floorMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("floor"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* ceilingMethodCallExpr = "ceiling" OPEN BWS commonExpr BWS CLOSE
named!(ceilingMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("ceiling"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//*
//* distanceMethodCallExpr   = "geo.distance"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(distanceMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("geo.distance"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* geoLengthMethodCallExpr  = "geo.length"     OPEN BWS commonExpr BWS CLOSE
named!(geoLengthMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("geo.length"), OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* intersectsMethodCallExpr = "geo.intersects" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(intersectsMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("geo.intersects"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//*
//* hasSubsetMethodCallExpr      = "hassubset"      OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsetMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("hassubset"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//* hasSubsequenceMethodCallExpr = "hassubsequence" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsequenceMethodCallExpr<&str, &str>, call!(recognize(tuple((tag_no_case("hassubsequence"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)))));
//*
//* parenExpr = OPEN BWS commonExpr BWS CLOSE
named!(parenExpr<&str, &str>, call!(recognize(tuple((OPEN, BWS, commonExpr, BWS, CLOSE)))));
//* listExpr  = OPEN BWS commonExpr BWS *( COMMA BWS commonExpr BWS ) CLOSE
named!(listExpr<&str, &str>, call!(recognize(tuple((OPEN, BWS, commonExpr, many0(tuple((COMMA, BWS, commonExpr, BWS))), CLOSE)))));
//*
//* andExpr = RWS "and" RWS boolCommonExpr
named!(andExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("and"), RWS, boolCommonExpr)))));
//* orExpr  = RWS "or"  RWS boolCommonExpr
named!(orExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("or"), RWS, boolCommonExpr)))));
//*
//* eqExpr = RWS "eq" RWS commonExpr
named!(eqExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("eq"), RWS, commonExpr)))));
//* neExpr = RWS "ne" RWS commonExpr
named!(neExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("ne"), RWS, commonExpr)))));
//* ltExpr = RWS "lt" RWS commonExpr
named!(ltExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("lt"), RWS, commonExpr)))));
//* leExpr = RWS "le" RWS commonExpr
named!(leExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("le"), RWS, commonExpr)))));
//* gtExpr = RWS "gt" RWS commonExpr
named!(gtExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("gt"), RWS, commonExpr)))));
//* geExpr = RWS "ge" RWS commonExpr
named!(geExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("ge"), RWS, commonExpr)))));
//* inExpr = RWS "in" RWS commonExpr
named!(inExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("in"), RWS, commonExpr)))));
//*
//* hasExpr = RWS "has" RWS enum
named!(hasExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("has"), RWS, commonExpr)))));
//*
//* addExpr   = RWS "add"   RWS commonExpr
named!(addExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("add"), RWS, commonExpr)))));
//* subExpr   = RWS "sub"   RWS commonExpr
named!(subExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("sub"), RWS, commonExpr)))));
//* mulExpr   = RWS "mul"   RWS commonExpr
named!(mulExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("mul"), RWS, commonExpr)))));
//* divExpr   = RWS "div"   RWS commonExpr
named!(divExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("div"), RWS, commonExpr)))));
//* divbyExpr = RWS "divby" RWS commonExpr
named!(divbyExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("divby"), RWS, commonExpr)))));
//* modExpr   = RWS "mod"   RWS commonExpr
named!(modExpr<&str, &str>, call!(recognize(tuple((RWS, tag_no_case("mod"), RWS, commonExpr)))));
//*
//* negateExpr = "-" BWS commonExpr
named!(negateExpr<&str, &str>, call!(recognize(tuple((tag("-"), BWS, commonExpr)))));
//*
//* notExpr = "not" RWS boolCommonExpr
named!(notExpr<&str, &str>, call!(recognize(tuple((tag_no_case("not"), RWS, boolCommonExpr)))));
//*
//* isofExpr = "isof" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(isofExpr<&str, &str>, call!(recognize(tuple((tag_no_case("isof"), OPEN, BWS, opt(tuple((commonExpr, BWS, COMMA, BWS))), qualifiedTypeName, BWS, CLOSE)))));
//* castExpr = "cast" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(castExpr<&str, &str>, call!(recognize(tuple((tag_no_case("cast"), OPEN, BWS, opt(tuple((commonExpr, BWS, COMMA, BWS))), qualifiedTypeName, BWS, CLOSE)))));
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
named!(arrayOrObject<&str, &str>, call!(alt((complexColInUri, complexInUri, rootExprCol, primitiveColInUri))));
//*
//* complexColInUri = begin-array
//*                   [ complexInUri *( value-separator complexInUri ) ]
//*                   end-array
named!(complexColInUri<&str, &str>, call!(recognize(tuple((begin_array, opt(tuple((complexInUri, many0(tuple((value_separator, complexInUri)))))), end_array)))));
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
named!(complexInUri<&str, &str>, call!(recognize(tuple((begin_object,
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
named!(collectionPropertyInUri<&str, &str>, call!(alt((recognize(tuple((quotation_mark, primitiveColProperty, quotation_mark, name_separator, primitiveColInUri)))
						 , recognize(tuple((quotation_mark, complexColProperty, quotation_mark, name_separator, complexColInUri)))))));

//*
//* primitiveColInUri = begin-array
//*                     [ primitiveLiteralInJSON *( value-separator primitiveLiteralInJSON ) ]
//*                     end-array
named!(primitiveColInUri<&str, &str>, call!(recognize(tuple((begin_array, opt(tuple((primitiveLiteralInJSON, many0(tuple((value_separator, primitiveLiteralInJSON)))))), end_array)))));
//*
//* complexPropertyInUri = quotation-mark complexProperty quotation-mark
//*                        name-separator
//*                        complexInUri
named!(complexPropertyInUri<&str, &str>, call!(recognize(tuple((quotation_mark, complexProperty, quotation_mark, name_separator, complexInUri)))));
//*
//* annotationInUri = quotation-mark AT namespace "." termName quotation-mark
//*                   name-separator
//*                   ( complexInUri / complexColInUri / primitiveLiteralInJSON / primitiveColInUri )
named!(annotationInUri<&str, &str>, call!(recognize(tuple((quotation_mark, AT, namespace, tag("."), termName, quotation_mark,
						      name_separator,
						      alt((complexInUri, complexColInUri, primitiveLiteralInJSON, primitiveColInUri)))))));
//*
//* primitivePropertyInUri = quotation-mark primitiveProperty quotation-mark
//*                          name-separator
//*                          primitiveLiteralInJSON
named!(primitivePropertyInUri<&str, &str>, call!(recognize(tuple((quotation_mark, primitiveProperty, quotation_mark, name_separator, primitiveLiteralInJSON)))));
//*
//* navigationPropertyInUri = singleNavPropInJSON
//*                         / collectionNavPropInJSON
named!(navigationPropertyInUri<&str, &str>, call!(alt((singleNavPropInJSON, collectionNavPropInJSON))));
//* singleNavPropInJSON     = quotation-mark entityNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExpr
named!(singleNavPropInJSON<&str, &str>, call!(recognize(tuple((quotation_mark, entityNavigationProperty, quotation_mark, name_separator, rootExpr)))));
//* collectionNavPropInJSON = quotation-mark entityColNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExprCol
named!(collectionNavPropInJSON<&str, &str>, call!(recognize(tuple((quotation_mark, entityColNavigationProperty, quotation_mark, name_separator, rootExprCol)))));
//*
//* rootExprCol = begin-array
//*               [ rootExpr *( value-separator rootExpr ) ]
//*               end-array
named!(rootExprCol<&str, &str>, call!(recognize(tuple((begin_array, opt(tuple((rootExpr, many0(tuple((value_separator, rootExpr)))))), end_array)))));
//*
//* ; JSON syntax: adapted to URI restrictions from [RFC4627]
//* begin-object = BWS ( "{" / "%7B" ) BWS
named!(begin_object<&str, &str>, call!(recognize(tuple((BWS, alt((tag("{"), tag("%7B"))))))));
//* end-object   = BWS ( "}" / "%7D" )
named!(end_object<&str, &str>, call!(recognize(tuple((BWS, alt((tag("}"), tag("%7D"))))))));
//*
//* begin-array = BWS ( "[" / "%5B" ) BWS
named!(begin_array<&str, &str>, call!(recognize(tuple((BWS, alt((tag("["), tag("%5B"))))))));
//* end-array   = BWS ( "]" / "%5D" )
named!(end_array<&str, &str>, call!(recognize(tuple((BWS, alt((tag("]"), tag("%5D"))))))));
//*
//* quotation-mark  = DQUOTE / "%22"
named!(quotation_mark<&str, &str>, call!(alt((recognize(DQUOTE), tag("%22")))));
//* name-separator  = BWS COLON BWS
named!(name_separator<&str, &str>, call!(recognize(tuple((BWS, COLON, BWS)))));
//* value-separator = BWS COMMA BWS
named!(value_separator<&str, &str>, call!(recognize(tuple((BWS, COMMA, BWS)))));
//*
//* primitiveLiteralInJSON = stringInJSON
//*                        / numberInJSON
//*                        / 'true'
//*                        / 'false'
//*                        / 'null'
named!(primitiveLiteralInJSON<&str, &str>, call!(alt((stringInJSON, numberInJSON, tag("true"), tag("false"), tag("null")))));
//*
//* stringInJSON = quotation-mark *charInJSON quotation-mark
named!(stringInJSON<&str, &str>, call!(recognize(tuple((quotation_mark, many0(charInJSON), quotation_mark)))));
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
named!(charInJSON<&str, &str>, call!(alt((qchar_unescaped, qchar_JSON_special, recognize(tuple((escape, alt((  quotation_mark
													  , escape
													  , alt((tag("/"), tag("%2F")))
													  , recognize(one_of("bfnrt"))
													  , recognize(tuple((tag("u"), many_m_n(4, 4, HEXDIG))))
													  )))))))));
//*
//* qchar-JSON-special = SP / ":" / "{" / "}" / "[" / "]" ; some agents put these unencoded into the query part of a URL
named!(qchar_JSON_special<&str, &str>, call!(alt((SP, recognize(one_of(":{}[]"))))));
//*
//* escape = "\" / "%5C"     ; reverse solidus U+005C
named!(escape<&str, &str>, call!(alt((tag("\\"), tag("%5C")))));
//*
//* numberInJSON = [ "-" ] int [ frac ] [ exp ]
named!(numberInJSON<&str, &str>, call!(recognize(tuple((opt(tag("-")), int, opt(frac), opt(exp))))));
//* int          = "0" / ( oneToNine *DIGIT )
named!(int<&str, &str>, call!(alt((tag("0"), recognize(tuple((oneToNine, many0(DIGIT))))))));
//* frac         = "." 1*DIGIT
named!(frac<&str, &str>, call!(recognize(tuple((tag("."), many1(DIGIT))))));
//* exp          = "e" [ "-" / "+" ] 1*DIGIT
named!(exp<&str, &str>, call!(recognize(tuple((tag_no_case("e"), opt(alt((tag("-"), tag("+")))), many1(DIGIT))))));
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
named!(singleQualifiedTypeName<&str, &str>, call!(alt((qualifiedEntityTypeName
						 , qualifiedComplexTypeName
						 , qualifiedTypeDefinitionName
						 , qualifiedEnumTypeName
						 , primitiveTypeName))));
//*
//* qualifiedTypeName = singleQualifiedTypeName
//*                   / 'Collection' OPEN singleQualifiedTypeName CLOSE
named!(qualifiedTypeName<&str, &str>, call!(alt((singleQualifiedTypeName
					   , recognize(tuple((tag("Collection"), OPEN, singleQualifiedTypeName, CLOSE)))))));
//*
//* qualifiedEntityTypeName     = namespace "." entityTypeName
named!(qualifiedEntityTypeName<&str, &str>, call!(recognize(tuple((namespace, tag("."), entityTypeName)))));
//* qualifiedComplexTypeName    = namespace "." complexTypeName
named!(qualifiedComplexTypeName<&str, &str>, call!(recognize(tuple((namespace, tag("."), complexTypeName)))));
//* qualifiedTypeDefinitionName = namespace "." typeDefinitionName
named!(qualifiedTypeDefinitionName<&str, &str>, call!(recognize(tuple((namespace, tag("."), typeDefinitionName)))));
//* qualifiedEnumTypeName       = namespace "." enumerationTypeName
named!(qualifiedEnumTypeName<&str, &str>, call!(recognize(tuple((namespace, tag("."), enumerationTypeName)))));
//*
//* ; an alias is just a single-part namespace
//* namespace     = namespacePart *( "." namespacePart )
named!(namespace<&str, &str>, call!(recognize(tuple((namespacePart, many0(tuple((tag("."), namespacePart))))))));
//* namespacePart = odataIdentifier
named!(namespacePart<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* entitySetName       = odataIdentifier
fn entitySetName_wip<'a>(input: &'a str, ctx: &Parser, container: &'a schema::EntityContainer) -> IResult<&'a str, &'a schema::EntitySet> {
	map_opt(recognize(odataIdentifier), |name: &str| {
		if let Some(schema::EntityContainerMember::EntitySet(member)) = container.members.get(name) {
			return Some(member);
		}
		return None;
	})(input)
}
named!(entitySetName<&str, &str>, call!(recognize(odataIdentifier)));
//* singletonEntity     = odataIdentifier
named!(singletonEntity<&str, &str>, call!(recognize(odataIdentifier)));
//* entityTypeName      = odataIdentifier
named!(entityTypeName<&str, &str>, call!(recognize(odataIdentifier)));
//* complexTypeName     = odataIdentifier
named!(complexTypeName<&str, &str>, call!(recognize(odataIdentifier)));
//* typeDefinitionName  = odataIdentifier
named!(typeDefinitionName<&str, &str>, call!(recognize(odataIdentifier)));
//* enumerationTypeName = odataIdentifier
named!(enumerationTypeName<&str, &str>, call!(recognize(odataIdentifier)));
//* enumerationMember   = odataIdentifier
named!(enumerationMember<&str, &str>, call!(recognize(odataIdentifier)));
//* termName            = odataIdentifier
named!(termName<&str, &str>, call!(recognize(odataIdentifier)));

//TODO(restrictive + unicode)
//* ; Note: this pattern is overly restrictive, the normative definition is type TSimpleIdentifier in OData EDM XML Schema
//* odataIdentifier             = identifierLeadingCharacter *127identifierCharacter
named!(odataIdentifier<&str, &str>, call!(recognize(tuple((identifierLeadingCharacter, many_m_n(0, 127, identifierCharacter))))));
//* identifierLeadingCharacter  = ALPHA / "_"         ; plus Unicode characters from the categories L or Nl
named!(identifierLeadingCharacter<&str, &str>, call!(alt((ALPHA, tag("_")))));
//* identifierCharacter         = ALPHA / "_" / DIGIT ; plus Unicode characters from the categories L, Nl, Nd, Mn, Mc, Pc, or Cf
named!(identifierCharacter<&str, &str>, call!(alt((ALPHA, tag("_"), DIGIT))));
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
named!(primitiveTypeName<&str, &str>, call!(recognize(tuple((tag("Edm."), alt((  tag("Binary")
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
named!(abstractSpatialTypeName<&str, &str>, call!(alt((tag("Geography"), tag("Geometry")))));
//* concreteSpatialTypeName = 'Collection'
//*                         / 'LineString'
//*                         / 'MultiLineString'
//*                         / 'MultiPoint'
//*                         / 'MultiPolygon'
//*                         / 'Point'
//*                         / 'Polygon'
named!(concreteSpatialTypeName<&str, &str>, call!(alt((  tag("Collection")
						 , tag("LineString")
						 , tag("MultiLineString")
						 , tag("MultiPoint")
						 , tag("MultiPolygon")
						 , tag("Point")
						 , tag("Polygon")
						 ))));
//*
//* primitiveProperty       = primitiveKeyProperty / primitiveNonKeyProperty
named!(primitiveProperty<&str, &str>, call!(alt((primitiveKeyProperty, primitiveNonKeyProperty))));
//* primitiveKeyProperty    = odataIdentifier
named!(primitiveKeyProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveNonKeyProperty = odataIdentifier
named!(primitiveNonKeyProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveColProperty    = odataIdentifier
named!(primitiveColProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* complexProperty         = odataIdentifier
named!(complexProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* complexColProperty      = odataIdentifier
named!(complexColProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* streamProperty          = odataIdentifier
named!(streamProperty<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* navigationProperty          = entityNavigationProperty / entityColNavigationProperty
named!(navigationProperty<&str, &str>, call!(alt((entityNavigationProperty, entityColNavigationProperty))));
//* entityNavigationProperty    = odataIdentifier
named!(entityNavigationProperty<&str, &str>, call!(recognize(odataIdentifier)));
//* entityColNavigationProperty = odataIdentifier
named!(entityColNavigationProperty<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* action       = odataIdentifier
named!(action<&str, &str>, call!(recognize(odataIdentifier)));
//* actionImport = odataIdentifier
named!(actionImport<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* function = entityFunction
//*          / entityColFunction
//*          / complexFunction
//*          / complexColFunction
//*          / primitiveFunction
//*          / primitiveColFunction
named!(function<&str, &str>, call!(alt(( entityFunction
				 , entityColFunction
				 , complexFunction
				 , complexColFunction
				 , primitiveFunction
				 , primitiveColFunction))));
//*
//* entityFunction       = odataIdentifier
named!(entityFunction<&str, &str>, call!(recognize(odataIdentifier)));
//* entityColFunction    = odataIdentifier
named!(entityColFunction<&str, &str>, call!(recognize(odataIdentifier)));
//* complexFunction      = odataIdentifier
named!(complexFunction<&str, &str>, call!(recognize(odataIdentifier)));
//* complexColFunction   = odataIdentifier
named!(complexColFunction<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveFunction    = odataIdentifier
named!(primitiveFunction<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveColFunction = odataIdentifier
named!(primitiveColFunction<&str, &str>, call!(recognize(odataIdentifier)));
//*
//* entityFunctionImport       = odataIdentifier
named!(entityFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
//* entityColFunctionImport    = odataIdentifier
named!(entityColFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
//* complexFunctionImport      = odataIdentifier
named!(complexFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
//* complexColFunctionImport   = odataIdentifier
named!(complexColFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveFunctionImport    = odataIdentifier
named!(primitiveFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
//* primitiveColFunctionImport = odataIdentifier
named!(primitiveColFunctionImport<&str, &str>, call!(recognize(odataIdentifier)));
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
named!(primitiveLiteral<&str, &str>, call!(alt((
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
named!(primitiveValue<&str, &str>, call!(alt((
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
named!(nullValue<&str, &str>, call!(tag("null")));
//*
//* ; base64url encoding according to http://tools.ietf.org/html/rfc4648#section-5
//* binary      = "binary" SQUOTE binaryValue SQUOTE
named!(binary<&str, &str>, call!(recognize(tuple((tag_no_case("binary"), SQUOTE, binaryValue, SQUOTE)))));
//* binaryValue = *(4base64char) [ base64b16  / base64b8 ]
named!(binaryValue<&str, &str>, call!(recognize(tuple((many0(many_m_n(4, 4, base64char)), opt(alt((base64b16, base64b8))))))));
//* base64b16   = 2base64char ( 'A' / 'E' / 'I' / 'M' / 'Q' / 'U' / 'Y' / 'c' / 'g' / 'k' / 'o' / 's' / 'w' / '0' / '4' / '8' )   [ "=" ]
named!(base64b16<&str, &str>, call!(recognize(tuple((many_m_n(2, 2, base64char), one_of("AEIMQUYcgkosw048"), opt(tag("=")))))));
//* base64b8    = base64char ( 'A' / 'Q' / 'g' / 'w' ) [ "==" ]
named!(base64b8<&str, &str>, call!(recognize(tuple((base64char, one_of("AQgw"), opt(tag("==")))))));
//* base64char  = ALPHA / DIGIT / "-" / "_"
named!(base64char<&str, &str>, call!(alt((ALPHA, DIGIT, tag("-"), tag("_")))));
//*
//* booleanValue = "true" / "false"
named!(booleanValue<&str, &str>, call!(alt((tag_no_case("true"), tag_no_case("false")))));
//*
//* decimalValue = [ SIGN ] 1*DIGIT [ "." 1*DIGIT ] [ "e" [ SIGN ] 1*DIGIT ] / nanInfinity
named!(decimalValue<&str, &str>, call!(alt((recognize(tuple((opt(SIGN),
							many1(DIGIT),
						   	opt(tuple((tag("."), many1(DIGIT)))),
						   	opt(tuple((tag_no_case("e"), opt(SIGN), many1(DIGIT))))
							)))
				      , nanInfinity))));
//* doubleValue  = decimalValue ; IEEE 754 binary64 floating-point number (15-17 decimal digits)
named!(doubleValue<&str, &str>, call!(recognize(decimalValue)));
//* singleValue  = decimalValue ; IEEE 754 binary32 floating-point number (6-9 decimal digits)
named!(singleValue<&str, &str>, call!(recognize(decimalValue)));
//* nanInfinity  = 'NaN' / '-INF' / 'INF'
named!(nanInfinity<&str, &str>, call!(alt((tag("NaN"), tag("-INF"), tag("INF")))));
//*
//* guidValue = 8HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 12HEXDIG
named!(guidValue<&str, &str>, call!(recognize(tuple((many_m_n(8, 8, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(4, 4, HEXDIG), tag("-"),
						many_m_n(12, 12, HEXDIG)
						)))));
//*
//* byteValue  = 1*3DIGIT           ; numbers in the range from 0 to 255
named!(byteValue<&str, &str>, call!(recognize(many_m_n(1, 3, DIGIT))));
//* sbyteValue = [ SIGN ] 1*3DIGIT  ; numbers in the range from -128 to 127
named!(sbyteValue<&str, &str>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 3, DIGIT))))));
//* int16Value = [ SIGN ] 1*5DIGIT  ; numbers in the range from -32768 to 32767
named!(int16Value<&str, &str>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 5, DIGIT))))));
//* int32Value = [ SIGN ] 1*10DIGIT ; numbers in the range from -2147483648 to 2147483647
named!(int32Value<&str, &str>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 10, DIGIT))))));
//* int64Value = [ SIGN ] 1*19DIGIT ; numbers in the range from -9223372036854775808 to 9223372036854775807
named!(int64Value<&str, &str>, call!(recognize(tuple((opt(SIGN), many_m_n(1, 19, DIGIT))))));
//*
//* string           = SQUOTE *( SQUOTE-in-string / pchar-no-SQUOTE ) SQUOTE
// errata: pchar-no-SQUOTE includes special characters like &, =, and $. Those should be encoded
named!(string<&str, &str>, call!(recognize(tuple((SQUOTE, many0(alt((recognize(SQUOTE_in_string), recognize(pchar_no_SQUOTE)))), SQUOTE)))));
//* SQUOTE-in-string = SQUOTE SQUOTE ; two consecutive single quotes represent one within a string literal
named!(SQUOTE_in_string<&str, &str>, call!(recognize(tuple((SQUOTE, SQUOTE)))));
//*
//* dateValue = year "-" month "-" day
named!(dateValue<&str, &str>, call!(recognize(tuple((year, tag("-"), month, tag("-"), day)))));
//*
//* dateTimeOffsetValue = year "-" month "-" day "T" hour ":" minute [ ":" second [ "." fractionalSeconds ] ] ( "Z" / SIGN hour ":" minute )
named!(dateTimeOffsetValue<&str, &str>, call!(recognize(tuple((year, tag("-"), month, tag("-"), day, tag_no_case("T"), hour, tag(":"), minute,
							  opt(tuple((tag(":"), second, opt(tuple((tag("."), fractionalSeconds)))))),
							  alt((tag_no_case("Z"), recognize(tuple((SIGN, hour, tag(":"), minute)))))
							  )))));
//*
//* duration      = [ "duration" ] SQUOTE durationValue SQUOTE
named!(duration<&str, &str>, call!(recognize(tuple((opt(tag_no_case("duration")), SQUOTE, durationValue, SQUOTE)))));
//* durationValue = [ SIGN ] "P" [ 1*DIGIT "D" ] [ "T" [ 1*DIGIT "H" ] [ 1*DIGIT "M" ] [ 1*DIGIT [ "." 1*DIGIT ] "S" ] ]
//*      ; the above is an approximation of the rules for an xml dayTimeDuration.
//*      ; see the lexical representation for dayTimeDuration in http://www.w3.org/TR/xmlschema11-2#dayTimeDuration for more information
named!(durationValue<&str, &str>, call!(recognize(tuple((opt(SIGN),
						    tag_no_case("P"),
						    opt(tuple((many1(DIGIT), tag_no_case("D")))),
						    opt(tuple((tag_no_case("T"),
								opt(tuple((many1(DIGIT), tag_no_case("H")))),
								opt(tuple((many1(DIGIT), tag_no_case("M")))),
								opt(tuple((many1(DIGIT), opt(tuple((tag("."), many1(DIGIT)))), tag_no_case("S"))))
								))))))));

//*
//* timeOfDayValue = hour ":" minute [ ":" second [ "." fractionalSeconds ] ]
named!(timeOfDayValue<&str, &str>, call!(recognize(tuple((hour, tag(":"), minute, opt(tuple((tag(":"), second, opt(tuple((tag("."), fractionalSeconds)))))))))));
//*
//* oneToNine       = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
named!(oneToNine<&str, &str>, call!(recognize(one_of("123456789"))));
//* zeroToFiftyNine = ( "0" / "1" / "2" / "3" / "4" / "5" ) DIGIT
named!(zeroToFiftyNine<&str, &str>, call!(recognize(tuple((one_of("012345"), DIGIT)))));
//* year  = [ "-" ] ( "0" 3DIGIT / oneToNine 3*DIGIT )
named!(year<&str, &str>, call!(recognize(tuple((opt(tag("-")), alt((recognize(tuple((tag("0"), many_m_n(3, 3, DIGIT)))), recognize(tuple((oneToNine, many_m_n(3, 3, DIGIT)))))))))));
//* month = "0" oneToNine
//*       / "1" ( "0" / "1" / "2" )
named!(month<&str, &str>, call!(alt((  recognize(tuple((tag("0"), oneToNine)))
			       , recognize(tuple((tag("1"), one_of("012"))))
			       ))));
//* day   = "0" oneToNine
//*       / ( "1" / "2" ) DIGIT
//*       / "3" ( "0" / "1" )
named!(day<&str, &str>, call!(alt((  recognize(tuple((tag("0"), oneToNine)))
			     , recognize(tuple((one_of("12"), DIGIT)))
			     , recognize(tuple((tag("3"), one_of("01"))))
			     ))));
//* hour   = ( "0" / "1" ) DIGIT
//*        / "2" ( "0" / "1" / "2" / "3" )
named!(hour<&str, &str>, call!(alt((  recognize(tuple((one_of("01"), DIGIT)))
			      , recognize(tuple((tag("2"), one_of("0123"))))
			      ))));
//* minute = zeroToFiftyNine
named!(minute<&str, &str>, call!(recognize(zeroToFiftyNine)));
//* second = zeroToFiftyNine
named!(second<&str, &str>, call!(recognize(zeroToFiftyNine)));
//* fractionalSeconds = 1*12DIGIT
named!(fractionalSeconds<&str, &str>, call!(recognize(many_m_n(1, 12, DIGIT))));
//*
//* enum            = [ qualifiedEnumTypeName ] SQUOTE enumValue SQUOTE
named!(_enum<&str, &str>, call!(recognize(tuple((opt(qualifiedEntityTypeName), SQUOTE, enumValue, SQUOTE)))));
//* enumValue       = singleEnumValue *( COMMA singleEnumValue )
named!(enumValue<&str, &str>, call!(recognize(tuple((singleEnumValue, many0(tuple((COMMA, singleEnumValue))))))));
//* singleEnumValue = enumerationMember / enumMemberValue
named!(singleEnumValue<&str, &str>, call!(alt((enumerationMember, enumMemberValue))));
//* enumMemberValue = int64Value
named!(enumMemberValue<&str, &str>, call!(recognize(int64Value)));


//* geographyCollection   = geographyPrefix SQUOTE fullCollectionLiteral SQUOTE
named!(geographyCollection<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)))));
//* fullCollectionLiteral = sridLiteral collectionLiteral
named!(fullCollectionLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, collectionLiteral)))));
//* collectionLiteral     = "Collection(" geoLiteral *( COMMA geoLiteral ) CLOSE
named!(collectionLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("Collection("), geoLiteral, many0(tuple((COMMA, geoLiteral))), CLOSE)))));
//* geoLiteral            = collectionLiteral
//*                       / lineStringLiteral
//*                       / multiPointLiteral
//*                       / multiLineStringLiteral
//*                       / multiPolygonLiteral
//*                       / pointLiteral
//*                       / polygonLiteral
named!(geoLiteral<&str, &str>, call!(alt((  collectionLiteral
				    , lineStringLiteral
				    , multiPointLiteral
				    , multiLineStringLiteral
				    , multiPolygonLiteral
				    , pointLiteral
				    , polygonLiteral))));
//*
//* geographyLineString   = geographyPrefix SQUOTE fullLineStringLiteral SQUOTE
named!(geographyLineString<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)))));
//* fullLineStringLiteral = sridLiteral lineStringLiteral
named!(fullLineStringLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, lineStringLiteral)))));
//* lineStringLiteral     = "LineString" lineStringData
named!(lineStringLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("LineString"), lineStringData)))));
//* lineStringData        = OPEN positionLiteral 1*( COMMA positionLiteral ) CLOSE
named!(lineStringData<&str, &str>, call!(recognize(tuple((OPEN, positionLiteral, many1(tuple((COMMA, positionLiteral))), CLOSE)))));
//*
//* geographyMultiLineString   = geographyPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geographyMultiLineString<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)))));
//* fullMultiLineStringLiteral = sridLiteral multiLineStringLiteral
named!(fullMultiLineStringLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, multiLineStringLiteral)))));
//* multiLineStringLiteral     = "MultiLineString(" [ lineStringData *( COMMA lineStringData ) ] CLOSE
named!(multiLineStringLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("MultiLineString("), opt(tuple((lineStringData, many0(tuple((COMMA, lineStringData)))))), CLOSE)))));
//*
//* geographyMultiPoint   = geographyPrefix SQUOTE fullMultiPointLiteral SQUOTE
named!(geographyMultiPoint<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)))));
//* fullMultiPointLiteral = sridLiteral multiPointLiteral
named!(fullMultiPointLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, multiPointLiteral)))));
//* multiPointLiteral     = "MultiPoint(" [ pointData *( COMMA pointData ) ] CLOSE
named!(multiPointLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("MultiPoint("), opt(tuple((pointData, many0(tuple((COMMA, pointData)))))), CLOSE)))));
//*
//* geographyMultiPolygon   = geographyPrefix SQUOTE fullMultiPolygonLiteral SQUOTE
named!(geographyMultiPolygon<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)))));
//* fullMultiPolygonLiteral = sridLiteral multiPolygonLiteral
named!(fullMultiPolygonLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, multiPolygonLiteral)))));
//* multiPolygonLiteral     = "MultiPolygon(" [ polygonData *( COMMA polygonData ) ] CLOSE
named!(multiPolygonLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("MultiPolygon("), opt(tuple((polygonData, many0(tuple((COMMA, polygonData)))))), CLOSE)))));
//*
//* geographyPoint   = geographyPrefix SQUOTE fullPointLiteral SQUOTE
named!(geographyPoint<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullPointLiteral, SQUOTE)))));
//* fullPointLiteral = sridLiteral pointLiteral
named!(fullPointLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, pointLiteral)))));
//* sridLiteral      = "SRID" EQ 1*5DIGIT SEMI
named!(sridLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("SRID"), EQ, many_m_n(1, 5, DIGIT), SEMI)))));
//* pointLiteral     ="Point" pointData
named!(pointLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("Point"), pointData)))));
//* pointData        = OPEN positionLiteral CLOSE
named!(pointData<&str, &str>, call!(recognize(tuple((OPEN, positionLiteral, CLOSE)))));
//* positionLiteral  = doubleValue SP doubleValue  ; longitude, then latitude
named!(positionLiteral<&str, &str>, call!(recognize(tuple((doubleValue, SP, doubleValue)))));
//*
//* geographyPolygon   = geographyPrefix SQUOTE fullPolygonLiteral SQUOTE
named!(geographyPolygon<&str, &str>, call!(recognize(tuple((geographyPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)))));
//* fullPolygonLiteral = sridLiteral polygonLiteral
named!(fullPolygonLiteral<&str, &str>, call!(recognize(tuple((sridLiteral, polygonLiteral)))));
//* polygonLiteral     = "Polygon" polygonData
named!(polygonLiteral<&str, &str>, call!(recognize(tuple((tag_no_case("Polygon"), polygonData)))));
//* polygonData        = OPEN ringLiteral *( COMMA ringLiteral ) CLOSE
named!(polygonData<&str, &str>, call!(recognize(tuple((OPEN, ringLiteral, many0(tuple((COMMA, ringLiteral))), CLOSE)))));
//* ringLiteral        = OPEN positionLiteral *( COMMA positionLiteral ) CLOSE
//*                    ; Within each ringLiteral, the first and last positionLiteral elements MUST be an exact syntactic match to each other.
//*                    ; Within the polygonData, the ringLiterals MUST specify their points in appropriate winding order.
//*                    ; In order of traversal, points to the left side of the ring are interpreted as being in the polygon.
named!(ringLiteral<&str, &str>, call!(recognize(tuple((OPEN, positionLiteral, many0(tuple((COMMA, positionLiteral))), CLOSE)))));
//*
//* geometryCollection      = geometryPrefix SQUOTE fullCollectionLiteral      SQUOTE
named!(geometryCollection<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)))));
//* geometryLineString      = geometryPrefix SQUOTE fullLineStringLiteral      SQUOTE
named!(geometryLineString<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)))));
//* geometryMultiLineString = geometryPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geometryMultiLineString<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)))));
//* geometryMultiPoint      = geometryPrefix SQUOTE fullMultiPointLiteral      SQUOTE
named!(geometryMultiPoint<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)))));
//* geometryMultiPolygon    = geometryPrefix SQUOTE fullMultiPolygonLiteral    SQUOTE
named!(geometryMultiPolygon<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)))));
//* geometryPoint           = geometryPrefix SQUOTE fullPointLiteral           SQUOTE
named!(geometryPoint<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullPointLiteral, SQUOTE)))));
//* geometryPolygon         = geometryPrefix SQUOTE fullPolygonLiteral         SQUOTE
named!(geometryPolygon<&str, &str>, call!(recognize(tuple((geometryPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)))));
//*
//* geographyPrefix = "geography"
named!(geographyPrefix<&str, &str>, call!(tag_no_case("geography")));
//* geometryPrefix  = "geometry"
named!(geometryPrefix<&str, &str>, call!(tag_no_case("geometry")));
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
named!(obs_text<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && (*chr as u8) >= 0x80))));
//* ;quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
//*
//* OWS   = *( SP / HTAB )  ; "optional" whitespace
named!(OWS<&str, &str>, call!(recognize(many0(alt((SP, HTAB))))));
//* BWS-h = *( SP / HTAB )  ; "bad" whitespace in header values
named!(BWS_h<&str, &str>, call!(recognize(many0(alt((SP, HTAB))))));
//* EQ-h  = BWS-h EQ BWS-h
named!(EQ_h<&str, &str>, call!(recognize(tuple((BWS_h, EQ, BWS_h)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 9. Punctuation
//* ;------------------------------------------------------------------------------
//*
//* RWS = 1*( SP / HTAB / "%20" / "%09" )  ; "required" whitespace
named!(RWS<&str, &str>, call!(recognize(many1(alt((SP, HTAB, tag("%20"), tag("%09")))))));
//* BWS =  *( SP / HTAB / "%20" / "%09" )  ; "bad" whitespace
named!(BWS<&str, &str>, call!(recognize(many0(alt((SP, HTAB, tag("%20"), tag("%09")))))));
//*
//* AT     = "@" / "%40"
named!(AT<&str, &str>, call!(alt((tag("@"), tag("%40")))));
//* COLON  = ":" / "%3A"
named!(COLON<&str, &str>, call!(alt((tag(":"), tag("%3A")))));
//* COMMA  = "," / "%2C"
named!(COMMA<&str, &str>, call!(alt((tag(","), tag("%2C")))));
//* EQ     = "="
named!(EQ<&str, &str>, call!(tag("=")));
//* SIGN   = "+" / "%2B" / "-"
named!(SIGN<&str, &str>, call!(alt((tag("+"), tag("%3B"), tag("-")))));
//* SEMI   = ";" / "%3B"
named!(SEMI<&str, &str>, call!(alt((tag(";"), tag("%3B")))));
//* STAR   = "*" / "%2A"
named!(STAR<&str, &str>, call!(alt((tag("*"), tag("%2A")))));
//* SQUOTE = "'" / "%27"
named!(SQUOTE<&str, &str>, call!(alt((tag("'"), tag("%27")))));
//*
//* OPEN  = "(" / "%28"
named!(OPEN<&str, &str>, call!(alt((tag("("), tag("%28")))));
//* CLOSE = ")" / "%29"
named!(CLOSE<&str, &str>, call!(alt((tag(")"), tag("%29")))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; A. URI syntax [RFC3986]
//* ;------------------------------------------------------------------------------
//*
//* URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
named!(URI<&str, &str>, call!(recognize(tuple((scheme, tag(":"), hier_part, opt(tuple((tag("?"), query))), opt(tuple((tag("#"), fragment))))))));
//* hier-part     = "//" authority path-abempty
//*               / path-absolute
//*               / path-rootless
//* ;              / path-empty
named!(hier_part<&str, &str>, call!(recognize(tuple((tag("//"), authority, alt((path_abempty, path_absolute, path_rootless, path_empty)))))));
//* ;URI-reference = URI / relative-ref
//* ;absolute-URI  = scheme ":" hier-part [ "?" query ]
//* ;relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
//* ;relative-part = "//" authority path-abempty
//* ;              / path-absolute
//* ;              / path-noscheme
//* ;              / path-empty
//* scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
named!(scheme<&str, &str>, call!(recognize(tuple((ALPHA, many0(alt((ALPHA, DIGIT, tag("+"), tag("-"), tag(".")))))))));
//* authority     = [ userinfo "@" ] host [ ":" port ]
named!(authority<&str, &str>, call!(recognize(tuple((opt(tuple((userinfo, tag("@")))), host, opt(tuple((tag(":"), port))))))));
//* userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
named!(userinfo<&str, &str>, call!(recognize(many0(alt((unreserved, pct_encoded, sub_delims, tag(":")))))));
//* host          = IP-literal / IPv4address / reg-name
named!(host<&str, &str>, call!(alt((IP_literal, IPv4address, reg_name))));
//* port          = *DIGIT
named!(port<&str, &str>, call!(recognize(many0(DIGIT))));
//* IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
named!(IP_literal<&str, &str>, call!(delimited(tag("["), alt((IPv6address, IPvFuture)), tag("]"))));
//* IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
named!(IPvFuture<&str, &str>, call!(recognize(tuple((tag("v"), many1(HEXDIG), tag("."), many1(alt((unreserved, sub_delims, tag(":")))))))));
//* IPv6address   =                            6( h16 ":" ) ls32
//*                  /                       "::" 5( h16 ":" ) ls32
//*                  / [               h16 ] "::" 4( h16 ":" ) ls32
//*                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
//*                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
//*                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
//*                  / [ *4( h16 ":" ) h16 ] "::"              ls32
//*                  / [ *5( h16 ":" ) h16 ] "::"              h16
//*                  / [ *6( h16 ":" ) h16 ] "::"
named!(IPv6address<&str, &str>, call!(alt((
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
named!(h16<&str, &str>, call!(recognize(many1(many_m_n(4, 4, HEXDIG)))));
//* ls32          = ( h16 ":" h16 ) / IPv4address
named!(ls32<&str, &str>, call!(alt((recognize(separated_pair(h16, tag(":"), h16)), IPv4address))));
//* IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
named!(IPv4address<&str, &str>, call!(recognize(tuple((dec_octet, tag("."), dec_octet, tag("."), dec_octet, tag("."), dec_octet)))));
//* dec-octet     = "1" 2DIGIT            ; 100-199
//*               / "2" %x30-34 DIGIT     ; 200-249
//*               / "25" %x30-35          ; 250-255
//*               / %x31-39 DIGIT         ; 10-99
//*               / DIGIT                 ; 0-9
named!(dec_octet<&str, &str>, call!(alt((
	recognize(tuple((tag("1"), DIGIT, DIGIT)))            ,
	recognize(tuple((tag("2"), one_of("01234"), DIGIT))) ,
	recognize(tuple((tag("25"), one_of("012345"))))      ,
	recognize(tuple((one_of("123456789"), DIGIT)))        ,
	DIGIT
))));
//* reg-name      = *( unreserved / pct-encoded / sub-delims )
named!(reg_name<&str, &str>, call!(recognize(many0(alt((unreserved, pct_encoded, sub_delims))))));
//* ;path          = path-abempty    ; begins with "/" or is empty
//* ;              / path-absolute   ; begins with "/" but not "//"
//* ;              / path-noscheme   ; begins with a non-colon segment
//* ;              / path-rootless   ; begins with a segment
//* ;              / path-empty      ; zero characters
//* path-abempty  = *( "/" segment )
named!(path_abempty<&str, &str>, call!(recognize(many0(preceded(tag("/"), segment)))));
//* path-absolute = "/" [ segment-nz *( "/" segment ) ]
named!(path_absolute<&str, &str>, call!(recognize(tuple((tag("/"), opt(tuple((segment_nz, path_abempty))))))));
//* ;path-noscheme = segment-nz-nc *( "/" segment )
//* path-rootless = segment-nz *( "/" segment )
named!(path_rootless<&str, &str>, call!(recognize(tuple((segment_nz, path_abempty)))));
//* ;path-empty    = ""
named!(path_empty<&str, &str>, call!(tag("")));
//* segment       = *pchar
named!(segment<&str, &str>, call!(recognize(many0(pchar))));
//* segment-nz    = 1*pchar
named!(segment_nz<&str, &str>, call!(recognize(many1(pchar))));
//* ;segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
//* pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
named!(pchar<&str, &str>, call!(alt((unreserved, pct_encoded, sub_delims, recognize(one_of(":@"))))));
//* query         = *( pchar / "/" / "?" )
named!(query<&str, &str>, call!(recognize(many0(alt((pchar, recognize(one_of("/?"))))))));
//* fragment      = *( pchar / "/" / "?" )
named!(fragment<&str, &str>, call!(recognize(many0(alt((pchar, recognize(one_of("/?"))))))));
//* pct-encoded   = "%" HEXDIG HEXDIG
named!(pct_encoded<&str, &str>, call!(recognize(tuple((tag("%"), HEXDIG, HEXDIG)))));
//* unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
named!(unreserved<&str, &str>, call!(alt((ALPHA, DIGIT, recognize(one_of("-._~"))))));
//* ;reserved      = gen-delims / sub-delims
//* ;gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
//* ;sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
//* sub-delims     =       "$" / "&" / "'" /                                     "=" / other-delims
named!(sub_delims<&str, &str>, call!(alt((recognize(one_of("$&'=")), other_delims))));
//* other-delims   = "!" /                   "(" / ")" / "*" / "+" / "," / ";"
named!(other_delims<&str, &str>, call!(recognize(one_of("!()*+,;"))));
//*
//* pchar-no-SQUOTE       = unreserved / pct-encoded-no-SQUOTE / other-delims / "$" / "&" / "=" / ":" / "@"
named!(pchar_no_SQUOTE<&str, &str>, call!(alt((unreserved, pct_encoded_no_SQUOTE, other_delims, recognize(one_of("$&=:@"))))));
//* pct-encoded-no-SQUOTE = "%" ( "0" / "1" /   "3" / "4" / "5" / "6" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" / "2" / "3" / "4" / "5" / "6" /   "8" / "9" / A-to-F )
named!(pct_encoded_no_SQUOTE<&str, &str>, call!(alt((  recognize(tuple((tag("%"), one_of("013456789ABCDEFabcdef"), HEXDIG)))
					       , recognize(tuple((tag("%2"), one_of("012345689ABCDEFabcdef"))))
					      ))));
//*
//* qchar-no-AMP              = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_no_AMP<&str, &str>, call!(alt((qchar_no_AMP_EQ_AT_DOLLAR, tag("=")))));
//* qchar-no-AMP-EQ           = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'"
named!(qchar_no_AMP_EQ<&str, &str>, call!(alt((qchar_no_AMP_EQ_AT_DOLLAR, tag("@"), tag("$")))));
//* qchar-no-AMP-EQ-AT-DOLLAR = unreserved / pct-encoded / other-delims / ":" /       "/" / "?" /       "'"
named!(qchar_no_AMP_EQ_AT_DOLLAR<&str, &str>, call!(alt((unreserved, pct_encoded, other_delims, recognize(one_of(":/?'"))))));
//*
//* qchar-unescaped       = unreserved / pct-encoded-unescaped / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_unescaped<&str, &str>, call!(alt((unreserved, pct_encoded_unscaped, other_delims, recognize(one_of(":@/?$'="))))));
//* pct-encoded-unescaped = "%" ( "0" / "1" /   "3" / "4" /   "6" / "7" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" /   "3" / "4" / "5" / "6" / "7" / "8" / "9" / A-to-F )
//*                       / "%" "5" ( DIGIT / "A" / "B" /   "D" / "E" / "F" )
named!(pct_encoded_unscaped<&str, &str>, call!(alt(( recognize(tuple((tag("%"), alt((recognize(one_of("01346789")), A_to_F)), HEXDIG)))
					     , recognize(tuple((tag("%2"), alt((recognize(one_of("013456789")), A_to_F)))))
					     , recognize(tuple((tag("%5"), alt((DIGIT, recognize(one_of("ABDEFabdef")))))))
					     ))));

//*
//* qchar-no-AMP-DQUOTE   = qchar-unescaped
//*                       / escape ( escape / quotation-mark )
named!(qchar_no_AMP_DQUOTE<&str, &str>, call!(alt((qchar_unescaped, recognize(tuple((escape, alt((escape, quotation_mark)))))))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; B. IRI syntax [RFC3987]
//* ;------------------------------------------------------------------------------
//* ; Note: these are over-generous stubs, for the actual patterns refer to RFC3987
//* ;------------------------------------------------------------------------------
//*
//* IRI-in-header = 1*( VCHAR / obs-text )
named!(IRI_in_header<&str, &str>, call!(recognize(many1(alt((VCHAR, obs_text))))));
//* IRI-in-query  = 1*qchar-no-AMP
named!(IRI_in_query<&str, &str>, call!(recognize(many1(qchar_no_AMP))));

//* ;------------------------------------------------------------------------------
//* ; C. ABNF core definitions [RFC5234]
//* ;------------------------------------------------------------------------------
//*
//* ALPHA  = %x41-5A / %x61-7A
named!(ALPHA<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_alphabetic(*chr as u8)))));

//* DIGIT  = %x30-39
named!(DIGIT<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_digit(*chr as u8)))));
//
// //* HEXDIG = DIGIT / A-to-F
named!(HEXDIG<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_hex_digit(*chr as u8)))));

//* A-to-F = "A" / "B" / "C" / "D" / "E" / "F"
fn is_A_to_F(chr: u8) -> bool {
	(chr >= 0x41 && chr <= 0x46) || (chr >= 0x61 && chr <= 0x66)
}
named!(A_to_F<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii() && is_A_to_F(*chr as u8)))));

//* DQUOTE = %x22
named!(DQUOTE<&str, &str>, call!(tag("\u{0022}")));

//* SP     = %x20
named!(SP<&str, &str>, call!(tag("\u{0020}")));

//* HTAB   = %x09
named!(HTAB<&str, &str>, call!(tag("\u{0009}")));

//* ;WSP    = SP / HTAB
//* ;LWSP = *(WSP / CRLF WSP)
//* VCHAR = %x21-7E
named!(VCHAR<&str, &str>, call!(recognize(verify(anychar, |chr: &char| chr.is_ascii_graphic()))));

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
