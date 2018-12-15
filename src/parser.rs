#![allow(non_snake_case)]

use std::str;
use super::ast;

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
named!(odataUri<&str, &str>, recognize!(tuple!(serviceRoot, opt!(odataRelativeUri))));
//*
//* serviceRoot = ( "https" / "http" )                    ; Note: case-insensitive
//*               "://" host [ ":" port ]
//*               "/" *( segment-nz "/" )
named!(serviceRoot<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("https") | tag_no_case!("http")),
						  tag!("://"), host, opt!(tuple!(tag!(":"), port)),
						  tag!("/"), many0!(tuple!(segment_nz, tag!("/")))
						  )));
//*
//* ; Note: dollar-prefixed path segments are case-sensitive!
//* odataRelativeUri = '$batch'  [ "?" batchOptions ]
//*                  / '$entity' "?" entityOptions
//*                  / '$entity' "/" qualifiedEntityTypeName "?" entityCastOptions
//*                  / '$metadata' [ "?" metadataOptions ] [ context ]
//*                  / resourcePath [ "?" queryOptions ]
named!(pub odataRelativeUri<&str, ast::RelativeURI>, alt_complete!(
					  do_parse!(foo: preceded!(tag!("$batch"), opt!(preceded!(tag!("?"), batchOptions))) >> (ast::RelativeURI::Batch(foo)))
					  | value!(ast::RelativeURI::Entity, tuple!(tag!("$entity"), tag!("?"), entityOptions))
					  | value!(ast::RelativeURI::Entity, tuple!(tag!("$entity/"), qualifiedEntityTypeName, tag!("?"), entityCastOptions))
					  | value!(ast::RelativeURI::Metadata, tuple!(tag!("$metadata"), opt!(tuple!(tag!("?"), metadataOptions)), opt!(context)))
					  | value!(ast::RelativeURI::ResourcePath, tuple!(resourcePath, opt!(tuple!(tag!("?"), queryOptions))))
					  ));
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
named!(resourcePath<&str, &str>, alt_complete!(recognize!(tuple!(entitySetName, opt!(collectionNavigation)))
				      | recognize!(tuple!(singletonEntity,opt!(singleNavigation)))
				      | actionImportCall
				      | recognize!(tuple!(entityColFunctionImportCall, opt!(collectionNavigation)))
				      | recognize!(tuple!(entityFunctionImportCall, opt!(singleNavigation)))
				      | recognize!(tuple!(complexColFunctionImportCall, opt!(complexColPath)))
				      | recognize!(tuple!(complexFunctionImportCall, opt!(complexPath)))
				      | recognize!(tuple!(primitiveColFunctionImportCall, opt!(primitiveColPath)))
				      | recognize!(tuple!(primitiveFunctionImportCall, opt!(primitivePath)))
				      | functionImportCallNoParens
				      | crossjoin
				      | recognize!(tuple!(tag!("$all"), opt!(tuple!(tag!("/"), qualifiedEntityTypeName))))
				      ));
//*
//* collectionNavigation = [ "/" qualifiedEntityTypeName ] [ collectionNavPath ]
named!(collectionNavigation<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(collectionNavPath))));
//* collectionNavPath    = keyPredicate [ singleNavigation ]
//*                      / filterInPath [ collectionNavigation ]
//*                      / each [ boundOperation ]
//*                      / boundOperation
//*                      / count
//*                      / ref
named!(collectionNavPath<&str, &str>, alt_complete!(recognize!(tuple!(keyPredicate, opt!(singleNavigation)))
					   | recognize!(tuple!(filterInPath, opt!(collectionNavigation)))
					   | recognize!(tuple!(each, opt!(boundOperation)))
					   | boundOperation
					   | count
					   | _ref
					));
//*
//* keyPredicate     = simpleKey / compoundKey / keyPathSegments
named!(keyPredicate<&str, &str>, alt_complete!(simpleKey | compoundKey | keyPathSegments));
//* simpleKey        = OPEN ( parameterAlias / keyPropertyValue ) CLOSE
named!(simpleKey<&str, &str>, recognize!(tuple!(OPEN, alt_complete!(parameterAlias | keyPropertyValue), CLOSE)));
//* compoundKey      = OPEN keyValuePair *( COMMA keyValuePair ) CLOSE
named!(compoundKey<&str, &str>, recognize!(tuple!(OPEN, keyValuePair, many0!(tuple!(COMMA, keyValuePair)), CLOSE)));
//* keyValuePair     = ( primitiveKeyProperty / keyPropertyAlias  ) EQ ( parameterAlias / keyPropertyValue )
named!(keyValuePair<&str, &str>, recognize!(tuple!(alt_complete!(primitiveKeyProperty | keyPropertyAlias), EQ, alt_complete!(parameterAlias | keyPropertyValue))));
//* keyPropertyValue = primitiveLiteral
named!(keyPropertyValue<&str, &str>, recognize!(primitiveLiteral));
//* keyPropertyAlias = odataIdentifier
named!(keyPropertyAlias<&str, &str>, recognize!(odataIdentifier));
//* keyPathSegments  = 1*( "/" keyPathLiteral )
named!(keyPathSegments<&str, &str>, recognize!(many1!(tuple!(tag!("/"), keyPathLiteral))));
//* keyPathLiteral   = *pchar
named!(keyPathLiteral<&str, &str>, recognize!(many0!(pchar)));
//*
//* singleNavigation = [ "/" qualifiedEntityTypeName ]
//*                    [ "/" propertyPath
//*                    / boundOperation
//*                    / ref
//*                    / value  ; request the media resource of a media entity
//*                    ]
named!(singleNavigation<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(alt_complete!(recognize!(tuple!(tag!("/"), propertyPath))
														   | boundOperation
														   | _ref
														   | value
														   )))));
//*
//* propertyPath = entityColNavigationProperty [ collectionNavigation ]
//*              / entityNavigationProperty    [ singleNavigation ]
//*              / complexColProperty          [ complexColPath ]
//*              / complexProperty             [ complexPath ]
//*              / primitiveColProperty        [ primitiveColPath ]
//*              / primitiveProperty           [ primitivePath ]
//*              / streamProperty              [ boundOperation ]
named!(propertyPath<&str, &str>, recognize!(alt_complete!(tuple!(entityColNavigationProperty, opt!(collectionNavigation))
						 | tuple!(entityNavigationProperty, opt!(singleNavigation))
						 | tuple!(complexColProperty, opt!(complexColPath))
						 | tuple!(complexProperty, opt!(complexPath))
						 | tuple!(primitiveColProperty, opt!(primitiveColPath))
						 | tuple!(primitiveProperty, opt!(primitivePath))
						 | tuple!(streamProperty, opt!(boundOperation))
						 )));

//*
//* primitiveColPath = count / boundOperation / ordinalIndex
named!(primitiveColPath<&str, &str>, alt_complete!(count | boundOperation | ordinalIndex));
//*
//* primitivePath  = value / boundOperation
named!(primitivePath<&str, &str>, alt_complete!(value | boundOperation));
//*
//* complexColPath = ordinalIndex
//*                / [ "/" qualifiedComplexTypeName ] [ count / boundOperation ]
named!(complexColPath<&str, &str>, alt_complete!(ordinalIndex | recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(alt_complete!(count | boundOperation))))));
//*
//* complexPath    = [ "/" qualifiedComplexTypeName ]
//*                  [ "/" propertyPath
//*                  / boundOperation
//*                  ]
named!(complexPath<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(alt_complete!(recognize!(tuple!(tag!("/"), propertyPath)) | boundOperation)))));
//*
//* filterInPath = '/$filter' EQ parameterAlias
named!(filterInPath<&str, &str>, recognize!(tuple!(tag!("/$filter"), EQ, parameterAlias)));
//*
//* each  = '/$each'
named!(each<&str, &str>, tag!("/$each"));
//* count = '/$count'
named!(count<&str, &str>, tag!("/$count"));
//* ref   = '/$ref'
named!(_ref<&str, &str>, tag!("/$ref"));
//* value = '/$value'
named!(value<&str, &str>, tag!("/$value"));
//*
//* ordinalIndex = "/" 1*DIGIT
named!(ordinalIndex<&str, &str>, recognize!(tuple!(tag!("/"), many1!(DIGIT))));
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
named!(boundOperation<&str, &str>, recognize!(tuple!(tag!("/"), alt_complete!(boundActionCall
								     | recognize!(tuple!(boundEntityColFunctionCall, opt!(collectionNavigation)))
								     | recognize!(tuple!(boundEntityFunctionCall, opt!(singleNavigation)))
								     | recognize!(tuple!(boundComplexColFunctionCall, opt!(complexColPath)))
								     | recognize!(tuple!(boundComplexFunctionCall, opt!(complexPath)))
								     | recognize!(tuple!(boundPrimitiveColFunctionCall, opt!(primitiveColPath)))
								     | recognize!(tuple!(boundPrimitiveFunctionCall, opt!(primitivePath)))
								     | boundFunctionCallNoParens
								     ))));
//*
//* actionImportCall = actionImport
named!(actionImportCall<&str, &str>, recognize!(actionImport));
//* boundActionCall  = namespace "." action
//*                    ; with the added restriction that the binding parameter MUST be either an entity or collection of entities
//*                    ; and is specified by reference using the URI immediately preceding (to the left) of the boundActionCall
named!(boundActionCall<&str, &str>, recognize!(tuple!(namespace, tag!("."), action)));
//*
//* ; The following boundXxxFunctionCall rules have the added restrictions that
//* ;  - the function MUST support binding, and
//* ;  - the binding parameter type MUST match the type of resource identified by the
//* ;    URI immediately preceding (to the left) of the boundXxxFunctionCall, and
//* ;  - the functionParameters MUST NOT include the bindingParameter.
//TODO(validate)
//* boundEntityFunctionCall       = namespace "." entityFunction       functionParameters
named!(boundEntityFunctionCall<&str, &str>, recognize!(tuple!(entityFunction, functionParameters)));
//* boundEntityColFunctionCall    = namespace "." entityColFunction    functionParameters
named!(boundEntityColFunctionCall<&str, &str>, recognize!(tuple!(entityColFunction, functionParameters)));
//* boundComplexFunctionCall      = namespace "." complexFunction      functionParameters
named!(boundComplexFunctionCall<&str, &str>, recognize!(tuple!(complexFunction, functionParameters)));
//* boundComplexColFunctionCall   = namespace "." complexColFunction   functionParameters
named!(boundComplexColFunctionCall<&str, &str>, recognize!(tuple!(complexColFunction, functionParameters)));
//* boundPrimitiveFunctionCall    = namespace "." primitiveFunction    functionParameters
named!(boundPrimitiveFunctionCall<&str, &str>, recognize!(tuple!(primitiveFunction, functionParameters)));
//* boundPrimitiveColFunctionCall = namespace "." primitiveColFunction functionParameters
named!(boundPrimitiveColFunctionCall<&str, &str>, recognize!(tuple!(primitiveColFunction, functionParameters)));
//*
//* boundFunctionCallNoParens     = namespace "." entityFunction
//*                               / namespace "." entityColFunction
//*                               / namespace "." complexFunction
//*                               / namespace "." complexColFunction
//*                               / namespace "." primitiveFunction
//*                               / namespace "." primitiveColFunction
named!(boundFunctionCallNoParens<&str, &str>, recognize!(alt_complete!(tuple!(namespace, tag!("."), entityFunction)
							      | tuple!(namespace, tag!("."), entityColFunction)
						   	      | tuple!(namespace, tag!("."), complexFunction)
						   	      | tuple!(namespace, tag!("."), complexColFunction)
						   	      | tuple!(namespace, tag!("."), primitiveFunction)
						   	      | tuple!(namespace, tag!("."), primitiveColFunction)
							 )));

//*
//* entityFunctionImportCall       = entityFunctionImport       functionParameters
named!(entityFunctionImportCall<&str, &str>, recognize!(tuple!(entityFunctionImport,functionParameters)));
//* entityColFunctionImportCall    = entityColFunctionImport    functionParameters
named!(entityColFunctionImportCall<&str, &str>, recognize!(tuple!(entityColFunctionImport,functionParameters)));
//* complexFunctionImportCall      = complexFunctionImport      functionParameters
named!(complexFunctionImportCall<&str, &str>, recognize!(tuple!(complexFunctionImport,functionParameters)));
//* complexColFunctionImportCall   = complexColFunctionImport   functionParameters
named!(complexColFunctionImportCall<&str, &str>, recognize!(tuple!(complexColFunctionImport,functionParameters)));
//* primitiveFunctionImportCall    = primitiveFunctionImport    functionParameters
named!(primitiveFunctionImportCall<&str, &str>, recognize!(tuple!(primitiveFunctionImport,functionParameters)));
//* primitiveColFunctionImportCall = primitiveColFunctionImport functionParameters
named!(primitiveColFunctionImportCall<&str, &str>, recognize!(tuple!(primitiveColFunctionImport,functionParameters)));
//*
//* functionImportCallNoParens     = entityFunctionImport
//*                                / entityColFunctionImport
//*                                / complexFunctionImport
//*                                / complexColFunctionImport
//*                                / primitiveFunctionImport
//*                                / primitiveColFunctionImport
named!(functionImportCallNoParens<&str, &str>, alt_complete!(entityFunctionImport
						    | entityColFunctionImport
						    | complexFunctionImport
						    | complexColFunctionImport
						    | primitiveFunctionImport
						    | primitiveColFunctionImport));
//*
//* functionParameters = OPEN [ functionParameter *( COMMA functionParameter ) ] CLOSE
named!(functionParameters<&str, &str>, recognize!(tuple!(OPEN, opt!(tuple!(functionParameter, many0!(tuple!(COMMA, functionParameter)))), CLOSE)));
//* functionParameter  = parameterName EQ ( parameterAlias / primitiveLiteral )
named!(functionParameter<&str, &str>, recognize!(tuple!(parameterName, EQ, alt_complete!(parameterAlias | primitiveLiteral))));
//* parameterName      = odataIdentifier
named!(parameterName<&str, &str>, recognize!(odataIdentifier));
//* parameterAlias     = AT odataIdentifier
named!(parameterAlias<&str, &str>, recognize!(tuple!(AT, odataIdentifier)));
//*
//* crossjoin = '$crossjoin' OPEN
//*             entitySetName *( COMMA entitySetName )
//*             CLOSE
named!(crossjoin<&str, &str>, recognize!(tuple!(tag!("$crossjoin"), OPEN, entitySetName, many0!(tuple!(COMMA, entitySetName)), CLOSE)));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 2. Query Options
//* ;------------------------------------------------------------------------------
//*
//* queryOptions = queryOption *( "&" queryOption )
named!(queryOptions<&str, &str>, recognize!(tuple!(queryOption, many0!(tuple!(tag!("&"), queryOption)))));
//* queryOption  = systemQueryOption
//*              / aliasAndValue
//*              / nameAndValue
//*              / customQueryOption
named!(queryOption<&str, &str>, alt_complete!(systemQueryOption | aliasAndValue | nameAndValue | customQueryOption));
//*
//* batchOptions = batchOption *( "&" batchOption )
named!(batchOptions<&str, Vec<ast::QueryOption>>, separated_nonempty_list_complete!(tag!("&"), batchOption));
//* batchOption  = format
//*              /customQueryOption
named!(batchOption<&str, ast::QueryOption>, alt_complete!(format_wip | customQueryOption_wip));
//*
//* metadataOptions = metadataOption *( "&" metadataOption )
named!(metadataOptions<&str, &str>, recognize!(tuple!(metadataOption, many0!(tuple!(tag!("&"), metadataOption)))));
//* metadataOption  = format
//*                 /customQueryOption
named!(metadataOption<&str, &str>, alt_complete!(format | customQueryOption));
//*
//* entityOptions  = *( entityIdOption "&" ) id *( "&" entityIdOption )
named!(entityOptions<&str, &str>, recognize!(tuple!(many0!(tuple!(entityIdOption, tag!("&"))), id, many0!(tuple!(tag!("&"), entityIdOption)))));
//* entityIdOption = format
//*                / customQueryOption
named!(entityIdOption<&str, &str>, alt_complete!(format | customQueryOption));
//* entityCastOptions = *( entityCastOption "&" ) id *( "&" entityCastOption )
named!(entityCastOptions<&str, &str>, recognize!(tuple!(many0!(tuple!(entityCastOption, tag!("&"))), id, many0!(tuple!(tag!("&"), entityCastOption)))));
//* entityCastOption  = entityIdOption
//*                   / expand
//*                   / select
named!(entityCastOption<&str, &str>, alt_complete!(entityIdOption | expand | select));
//*
//* id = ( "$id" / "id" ) EQ IRI-in-query
named!(id<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$id") | tag_no_case!("id")), EQ, IRI_in_query)));
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
named!(systemQueryOption<&str, &str>, alt_complete!(compute
					   | deltatoken
					   | expand
					   | filter
					   | format
					   | id
					   | inlinecount
					   | orderby
					   | schemaversion
					   | search
					   | select
					   | skip
					   | skiptoken
					   | top
					   | index));
//*
//* compute          = ( "$compute" / "compute" ) EQ computeItem *( COMMA computeItem )
named!(compute<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$compute") | tag_no_case!("compute")), EQ, computeItem, many0!(tuple!(COMMA, computeItem)))));
//* computeItem      = commonExpr RWS "as" RWS computedProperty
named!(computeItem<&str, &str>, recognize!(tuple!(commonExpr, RWS, tag_no_case!("as"), RWS, computedProperty)));
//* computedProperty = odataIdentifier
named!(computedProperty<&str, &str>, recognize!(odataIdentifier));
//*
//* expand            = ( "$expand" / "expand" ) EQ expandItem *( COMMA expandItem )
named!(expand<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$expand") | tag_no_case!("expand")), EQ, expandItem, many0!(tuple!(COMMA, expandItem)))));
//* expandItem        = STAR [ ref / OPEN levels CLOSE ]
//*                   / "$value"
//*                   / expandPath
//*                     [ ref   [ OPEN expandRefOption   *( SEMI expandRefOption   ) CLOSE ]
//*                     / count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                     /         OPEN expandOption      *( SEMI expandOption      ) CLOSE
//*                     ]
named!(expandItem<&str, &str>, recognize!(alt_complete!(recognize!(tuple!(STAR, opt!(alt_complete!(_ref | recognize!(tuple!(OPEN, levels, CLOSE))))))
					       | tag_no_case!("$value")
					       | recognize!(tuple!(expandPath,
								   opt!(alt_complete!(
									     recognize!(tuple!(_ref, opt!(tuple!(OPEN, expandRefOption, many0!(tuple!(SEMI, expandRefOption)), CLOSE))))
									     | recognize!(tuple!(count, opt!(tuple!(OPEN, expandCountOption, many0!(tuple!(SEMI, expandCountOption)), CLOSE))))
									     | recognize!(tuple!(OPEN, expandOption, many0!(tuple!(SEMI, expandOption)), CLOSE))
									 ))
								  ))
						)));


//* expandPath        = [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                     *( ( complexProperty / complexColProperty ) "/" [ qualifiedComplexTypeName "/" ] )
//*                     ( STAR / streamProperty / navigationProperty [ "/" qualifiedEntityTypeName ] )
named!(expandPath<&str, &str>, recognize!(tuple!(
						opt!(tuple!(alt_complete!(qualifiedEntityTypeName | qualifiedComplexTypeName), tag!("/"))),
						many0!(tuple!(alt_complete!(complexProperty | complexColProperty), tag!("/"), opt!(tuple!(qualifiedComplexTypeName, tag!("/"))))),
						alt_complete!(STAR | streamProperty | recognize!(tuple!(navigationProperty, opt!(tuple!(tag!("/"), qualifiedEntityTypeName)))))
						)));

//* expandCountOption = filter
//*                   / search
named!(expandCountOption<&str, &str>, alt_complete!(filter | search));
//* expandRefOption   = expandCountOption
//*                   / orderby
//*                   / skip
//*                   / top
//*                   / inlinecount
named!(expandRefOption<&str, &str>, alt_complete!(expandCountOption | orderby | skip | top | inlinecount));
//* expandOption      = expandRefOption
//*                   / select
//*                   / expand
//*                   / compute
//*                   / levels
//*                   / aliasAndValue
named!(expandOption<&str, &str>, alt_complete!(expandRefOption | select | expand | compute | levels | aliasAndValue));
//*
//* levels = ( "$levels" / "levels" ) EQ ( oneToNine *DIGIT / "max" )
named!(levels<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$levels") | tag_no_case!("levels")), EQ, alt_complete!(recognize!(tuple!(oneToNine, many0!(DIGIT))) | tag_no_case!("max")))));
//*
//* filter = ( "$filter" / "filter" ) EQ boolCommonExpr
named!(filter<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$filter") | tag_no_case!("filter")), EQ, boolCommonExpr)));
//*
//* orderby     = ( "$orderby" / "orderby" ) EQ orderbyItem *( COMMA orderbyItem )
named!(orderby<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$orderby") | tag_no_case!("orderby")), EQ, orderbyItem, many0!(tuple!(COMMA, orderbyItem)))));
//* orderbyItem = commonExpr [ RWS ( "asc" / "desc" ) ]
named!(orderbyItem<&str, &str>, recognize!(tuple!(commonExpr, opt!(tuple!(RWS, alt_complete!(tag_no_case!("asc") | tag_no_case!("desc")))))));
//*
//* skip = ( "$skip" / "skip" ) EQ 1*DIGIT
named!(skip<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$skip") | tag_no_case!("skip")), EQ, many1!(DIGIT))));
//* top  = ( "$top"  / "top"  ) EQ 1*DIGIT
named!(top<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$top") | tag_no_case!("top")), EQ, many1!(DIGIT))));
//*
//* index  = ( "$index" / "index" ) EQ 1*DIGIT
named!(index<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$index") | tag_no_case!("index")), EQ, many1!(DIGIT))));
//*
//* format = ( "$format" / "format" ) EQ
//*          ( "atom"
//*          / "json"
//*          / "xml"
//*          / 1*pchar "/" 1*pchar ; <a data service specific value indicating a
//*          )                     ; format specific to the specific data service> or
//*                                ; <An IANA-defined [IANA-MMT] content type>
named!(format_wip<&str, ast::QueryOption>, preceded!(
					     tuple!(opt!(tag!("$")), tag_no_case!("format"), EQ),
					     do_parse!(
						foo: alt_complete!(
							     value!(ast::FormatKind::Atom, tag_no_case!("atom"))
							   | value!(ast::FormatKind::JSON, tag_no_case!("json"))
							   | value!(ast::FormatKind::XML, tag_no_case!("xml"))
							   | do_parse!(foo: recognize!(tuple!(many1!(pchar), tag!("/"), many1!(pchar))) >> (ast::FormatKind::Custom(foo)))
						) >>
						(ast::QueryOption::Format(foo))
					     )));
named!(format<&str, &str>, recognize!(format_wip));

//*
//* inlinecount = ( "$count" / "count" ) EQ booleanValue
named!(inlinecount<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$count") | tag_no_case!("count")), EQ, booleanValue)));
//*
//* schemaversion   = ( "$schemaversion" / "schemaversion" ) EQ ( STAR / 1*unreserved )
named!(schemaversion<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$schemaversion") | tag_no_case!("schemaversion")), EQ, alt_complete!(STAR | recognize!(many1!(unreserved))))));
//*
//* search     = ( "$search" / "search" ) EQ BWS searchExpr
named!(search<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$search") | tag_no_case!("search")), EQ, BWS, searchExpr)));
//* searchExpr = ( OPEN BWS searchExpr BWS CLOSE
//*              / searchTerm
//*              ) [ searchOrExpr
//*                / searchAndExpr
//*                ]
named!(searchExpr<&str, &str>, recognize!(tuple!(alt_complete!(recognize!(tuple!(OPEN, BWS, searchExpr, BWS, CLOSE)) | searchTerm), opt!(alt_complete!(searchOrExpr | searchAndExpr)))));
//*
//* searchOrExpr  = RWS 'OR'  RWS searchExpr
named!(searchOrExpr<&str, &str>, recognize!(tuple!(RWS, tag!("OR"), RWS, searchExpr)));
//* searchAndExpr = RWS [ 'AND' RWS ] searchExpr
named!(searchAndExpr<&str, &str>, recognize!(tuple!(RWS, opt!(tuple!(tag!("AND"), RWS)), searchExpr)));
//*
//* searchTerm   = [ 'NOT' RWS ] ( searchPhrase / searchWord )
named!(searchTerm<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("NOT"), RWS)), alt_complete!(searchPhrase | searchWord))));
//* searchPhrase = quotation-mark 1*qchar-no-AMP-DQUOTE quotation-mark
named!(searchPhrase<&str, &str>, recognize!(tuple!(quotation_mark, many1!(qchar_no_AMP_DQUOTE), quotation_mark)));
//*
//* ; A searchWord is a sequence of one or more letters, digits, commas, or dots.
//* ; This includes Unicode characters of categories L or N using UTF-8 and percent-encoding.
//* ; The words AND, OR, and NOT are not a valid searchWord.
//* ; Expressing this in ABNF is somewhat clumsy, so the following rule is overly generous.
//TODO(validation)
//* searchWord   = 1*( ALPHA / DIGIT / COMMA / "." / pct-encoded )
named!(searchWord<&str, &str>, recognize!(many1!(alt_complete!(ALPHA | DIGIT | COMMA | tag!(".") | pct_encoded))));
//*
//* select         = ( "$select" / "select" ) EQ selectItem *( COMMA selectItem )
named!(select<&str, &str>, recognize!(tuple!(alt_complete!(tag_no_case!("$select") | tag_no_case!("select")), EQ, selectItem, many0!(tuple!(COMMA, selectItem)))));
//* selectItem     = STAR
//*                / allOperationsInSchema
//*                / [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                  ( selectProperty
//*                  / qualifiedActionName
//*                  / qualifiedFunctionName
//*                  )
named!(selectItem<&str, &str>, alt_complete!(STAR
				    | allOperationsInSchema
				    | recognize!(tuple!(opt!(tuple!(alt_complete!(qualifiedEntityTypeName | qualifiedComplexTypeName), tag!("/"))),
							alt_complete!(selectProperty | qualifiedActionName | qualifiedFunctionName)
							))
				   ));
//* selectProperty = primitiveProperty
//*                / primitiveColProperty [ OPEN selectOptionPC *( SEMI selectOptionPC ) CLOSE ]
//*                / navigationProperty
//*                / selectPath [ OPEN selectOption *( SEMI selectOption ) CLOSE
//*                             / "/" selectProperty
//*                             ]
named!(selectProperty<&str, &str>, alt_complete!(primitiveProperty
					| recognize!(tuple!(primitiveColProperty, opt!(tuple!(OPEN, selectOptionPC, many0!(tuple!(SEMI, selectOptionPC)), CLOSE))))
					| navigationProperty
					| recognize!(tuple!(selectPath, opt!(alt_complete!(
										recognize!(tuple!(OPEN, selectOption, many0!(tuple!(SEMI, selectOption)), CLOSE))
										| recognize!(tuple!(tag!("/"), selectProperty))
										))))
					));
//* selectPath     = ( complexProperty / complexColProperty ) [ "/" qualifiedComplexTypeName ]
named!(selectPath<&str, &str>, recognize!(tuple!(alt_complete!(complexProperty | complexColProperty), opt!(tuple!(tag!("/"), qualifiedComplexTypeName)))));
//* selectOptionPC = filter / search / inlinecount / orderby / skip / top
named!(selectOptionPC<&str, &str>, alt_complete!(filter | search | inlinecount | orderby | skip | top));
//* selectOption   = selectOptionPC
//*                / compute / select / expand / aliasAndValue
named!(selectOption<&str, &str>, alt_complete!(selectOptionPC | compute | select | expand | aliasAndValue));
//*
//* allOperationsInSchema = namespace "." STAR
named!(allOperationsInSchema<&str, &str>, recognize!(tuple!(namespace, tag!("."), STAR)));
//*
//* ; The parameterNames uniquely identify the bound function overload
//* ; only if it has overloads.
//* qualifiedActionName   = namespace "." action
named!(qualifiedActionName<&str, &str>, recognize!(tuple!(namespace, tag!("."), action)));
//* qualifiedFunctionName = namespace "." function [ OPEN parameterNames CLOSE ]
named!(qualifiedFunctionName<&str, &str>, recognize!(tuple!(namespace, tag!("."), function, opt!(tuple!(OPEN, parameterNames, CLOSE)))));
//*
//* ; The names of all non-binding parameters, separated by commas
//* parameterNames = parameterName *( COMMA parameterName )
named!(parameterNames<&str, &str>, recognize!(tuple!(parameterName, many0!(tuple!(COMMA, parameterName)))));
//*
//* deltatoken = "$deltatoken" EQ 1*( qchar-no-AMP )
named!(deltatoken<&str, &str>, recognize!(tuple!(tag_no_case!("$deltatoken"), EQ, many1!(qchar_no_AMP))));
//*
//* skiptoken = "$skiptoken" EQ 1*( qchar-no-AMP )
named!(skiptoken<&str, &str>, recognize!(tuple!(tag_no_case!("$skiptoken"), EQ, many1!(qchar_no_AMP))));
//*
//* aliasAndValue = parameterAlias EQ parameterValue
named!(aliasAndValue<&str, &str>, recognize!(tuple!(parameterAlias, EQ, parameterValue)));
//*
//* nameAndValue = parameterName EQ parameterValue
named!(nameAndValue<&str, &str>, recognize!(tuple!(parameterName, EQ, parameterValue)));
//*
//* parameterValue = arrayOrObject
//*                / commonExpr
named!(parameterValue<&str, &str>, alt_complete!(arrayOrObject | commonExpr));
//*
//* customQueryOption = customName [ EQ customValue ]
named!(customQueryOption_wip<&str, ast::QueryOption>, do_parse!(foo: recognize!(tuple!(customName, opt!(tuple!(EQ, customValue)))) >> (ast::QueryOption::Custom(foo))));
named!(customQueryOption<&str, &str>, recognize!(customQueryOption_wip));
//* customName        = qchar-no-AMP-EQ-AT-DOLLAR *( qchar-no-AMP-EQ )
named!(customName<&str, &str>, recognize!(tuple!(qchar_no_AMP_EQ_AT_DOLLAR, many0!(qchar_no_AMP_EQ))));
//* customValue       = *( qchar-no-AMP )
named!(customValue<&str, &str>, recognize!(many0!(qchar_no_AMP)));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 3. Context URL Fragments
//* ;------------------------------------------------------------------------------
//*
//* context         = "#" contextFragment
named!(context<&str, &str>, recognize!(tuple!(tag!("#"), contextFragment)));
//* contextFragment = 'Collection($ref)'
//*                 / '$ref'
//*                 / 'Collection(Edm.EntityType)'
//*                 / 'Collection(Edm.ComplexType)'
//*                 / singletonEntity [ navigation *( containmentNavigation ) [ "/" qualifiedEntityTypeName ] ] [ selectList ]
//*                 / qualifiedTypeName [ selectList ]
//*                 / entitySet ( '/$deletedEntity' / '/$link' / '/$deletedLink' )
//*                 / entitySet keyPredicate "/" contextPropertyPath [ selectList ]
//*                 / entitySet [ selectList ] [ '/$entity' / '/$delta' ]
named!(contextFragment<&str, &str>, alt_complete!(tag!("Collection($ref)")
					 | tag!("$ref")
					 | tag!("Collection(Edm.EntityType)")
					 | tag!("Collection(Edm.ComplexType)")
					 | recognize!(tuple!(singletonEntity,
							     opt!(tuple!(navigation,
									 many0!(containmentNavigation),
									 opt!(tuple!(tag!("/"), qualifiedEntityTypeName))
							     )),
							     opt!(selectList)
					   ))
					 | recognize!(tuple!(qualifiedTypeName, opt!(selectList)))
					 | recognize!(tuple!(entitySet, alt_complete!(tag!("/$deletedEntity") | tag!("/$link") | tag!("/$deletedLink"))))
					 | recognize!(tuple!(entitySet, keyPredicate, tag!("/"), contextPropertyPath, opt!(selectList)))
					 | recognize!(tuple!(entitySet, opt!(selectList), opt!(alt_complete!(tag!("/$entity") | tag!("/$delta")))))
				    ));
//*
//* entitySet = entitySetName *( containmentNavigation ) [ "/" qualifiedEntityTypeName ]
named!(entitySet<&str, &str>, recognize!(tuple!(entitySetName, many0!(containmentNavigation), opt!(tuple!(tag!("/"), qualifiedEntityTypeName)))));
//*
//* containmentNavigation = keyPredicate [ "/" qualifiedEntityTypeName ] navigation
named!(containmentNavigation<&str, &str>, recognize!(tuple!(keyPredicate, opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), navigation)));
//* navigation            = *( "/" complexProperty [ "/" qualifiedComplexTypeName ] ) "/" navigationProperty
named!(navigation<&str, &str>, recognize!(tuple!(many0!(tuple!(tag!("/"), complexProperty, opt!(tuple!(tag!("/"), qualifiedComplexTypeName)))), tag!("/"), navigationProperty)));
//*
//* selectList         = OPEN selectListItem *( COMMA selectListItem ) CLOSE
named!(selectList<&str, &str>, recognize!(tuple!(OPEN, selectListItem, many0!(tuple!(COMMA, selectListItem)), CLOSE)));
//* selectListItem     = STAR ; all structural properties
//*                    / allOperationsInSchema
//*                    / [ qualifiedEntityTypeName "/" ]
//*                      ( qualifiedActionName
//*                      / qualifiedFunctionName
//*                      / selectListProperty
//*                      )
named!(selectListItem<&str, &str>, alt_complete!(STAR
					| allOperationsInSchema
					| recognize!(tuple!(opt!(tuple!(qualifiedEntityTypeName, tag!("/"))), alt_complete!(qualifiedActionName | qualifiedFunctionName | selectListProperty)))
				       ));
//* selectListProperty = primitiveProperty
//*                    / primitiveColProperty
//*                    / navigationProperty [ "+" ] [ selectList ]
//*                    / selectPath [ "/" selectListProperty ]
named!(selectListProperty<&str, &str>, alt_complete!(primitiveProperty
					    | primitiveColProperty
					    | recognize!(tuple!(navigationProperty, opt!(tag!("+")), opt!(selectList)))
					    | recognize!(tuple!(selectPath, opt!(tuple!(tag!("/"), selectListProperty))))
					   ));
//*
//* contextPropertyPath = primitiveProperty
//*                     / primitiveColProperty
//*                     / complexColProperty
//*                     / complexProperty [ [ "/" qualifiedComplexTypeName ] "/" contextPropertyPath ]
named!(contextPropertyPath<&str, &str>, alt_complete!(primitiveProperty
					     | primitiveColProperty
					     | complexColProperty
					     | recognize!(tuple!(complexProperty, opt!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), tag!("/"), contextPropertyPath))))
					     ));
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
named!(commonExpr<&str, &str>, recognize!(tuple!(
						 alt_complete!(
							 primitiveLiteral
							 | arrayOrObject
							 | rootExpr
							 | firstMemberExpr
							 | functionExpr
							 | negateExpr
							 | methodCallExpr
							 | parenExpr
							 | listExpr
							 | castExpr
							 | isofExpr
							 | notExpr
						 ),
						 opt!(alt_complete!(
							 addExpr
							 | subExpr
							 | mulExpr
							 | divExpr
							 | divbyExpr
							 | modExpr
						 )),
						 opt!(alt_complete!(
							 eqExpr
							 | neExpr
							 | ltExpr
							 | leExpr
							 | gtExpr
							 | geExpr
							 | hasExpr
							 | inExpr
						 )),
						 opt!(alt_complete!(andExpr | orExpr))
					)));
//*
//* boolCommonExpr = commonExpr ; resulting in a Boolean
//TODO(validate)
named!(boolCommonExpr<&str, &str>, recognize!(commonExpr));
//*
//* rootExpr = '$root/' ( entitySetName keyPredicate / singletonEntity ) [ singleNavigationExpr ]
named!(rootExpr<&str, &str>, recognize!(tuple!(tag!("$root/"), alt_complete!(recognize!(tuple!(entitySetName, keyPredicate)) | singletonEntity), opt!(singleNavigationExpr))));
//*
//* firstMemberExpr = memberExpr
//*                 / inscopeVariableExpr [ "/" memberExpr ]
named!(firstMemberExpr<&str, &str>, alt_complete!(memberExpr | recognize!(tuple!(inscopeVariableExpr, opt!(tuple!(tag!("/"), memberExpr))))));
//*
//* memberExpr = [ qualifiedEntityTypeName "/" ]
//*              ( propertyPathExpr
//*              / boundFunctionExpr
//*              / annotationExpr
//*              )
named!(memberExpr<&str, &str>, recognize!(tuple!(opt!(tuple!(qualifiedEntityTypeName, tag!("/"))), alt_complete!(propertyPathExpr | boundFunctionExpr | annotationExpr))));
//*
//* propertyPathExpr = ( entityColNavigationProperty [ collectionNavigationExpr ]
//*                    / entityNavigationProperty    [ singleNavigationExpr ]
//*                    / complexColProperty          [ complexColPathExpr ]
//*                    / complexProperty             [ complexPathExpr ]
//*                    / primitiveColProperty        [ collectionPathExpr ]
//*                    / primitiveProperty           [ primitivePathExpr ]
//*                    / streamProperty              [ primitivePathExpr ]
//*                    )
named!(propertyPathExpr<&str, &str>, recognize!(alt_complete!(
						     tuple!(entityColNavigationProperty, opt!(collectionNavigationExpr))
						     | tuple!(entityNavigationProperty, opt!(singleNavigationExpr))
						     | tuple!(complexColProperty, opt!(complexColPathExpr))
						     | tuple!(complexProperty, opt!(complexPathExpr))
						     | tuple!(primitiveColProperty, opt!(collectionPathExpr))
						     | tuple!(primitiveProperty, opt!(primitivePathExpr))
						     | tuple!(streamProperty, opt!(primitivePathExpr))
						)));
//*
//* annotationExpr = annotation
//*                  [ collectionPathExpr
//*                  / singleNavigationExpr
//*                  / complexPathExpr
//*                  / primitivePathExpr
//*                  ]
named!(annotationExpr<&str, &str>, recognize!(tuple!(annotation, opt!(alt_complete!(collectionPathExpr | singleNavigationExpr | complexPathExpr | primitivePathExpr)))));
//*
//* annotation          = AT [ namespace "." ] termName [ '#' annotationQualifier ]
named!(annotation<&str, &str>, recognize!(tuple!(AT, opt!(tuple!(namespace, tag!("."))), termName, opt!(tuple!(tag!("#"), annotationQualifier)))));
//* annotationQualifier = odataIdentifier
named!(annotationQualifier<&str, &str>, recognize!(odataIdentifier));
//*
//* inscopeVariableExpr  = implicitVariableExpr
//*                      / parameterAlias
//*                      / lambdaVariableExpr ; only allowed inside a lambdaPredicateExpr
//TODO(validation)
named!(inscopeVariableExpr<&str, &str>, alt_complete!(implicitVariableExpr | parameterAlias | lambdaVariableExpr));
//* implicitVariableExpr = '$it'              ; the current instance of the resource identified by the resource path
//*                      / '$this'            ; the instance on which the query option is evaluated
named!(implicitVariableExpr<&str, &str>, alt_complete!(tag!("$it") | tag!("$this")));
//* lambdaVariableExpr   = odataIdentifier
named!(lambdaVariableExpr<&str, &str>, recognize!(odataIdentifier));
//*
//* collectionNavigationExpr = [ "/" qualifiedEntityTypeName ]
//*                            [ keyPredicate [ singleNavigationExpr ]
//*                            / collectionPathExpr
//*                            ]
named!(collectionNavigationExpr<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(alt_complete!(
															   recognize!(tuple!(keyPredicate, opt!(singleNavigationExpr)))
															   | collectionPathExpr
															)))));
//*
//* singleNavigationExpr = "/" memberExpr
named!(singleNavigationExpr<&str, &str>, recognize!(tuple!(tag!("/"), memberExpr)));
//*
//* complexColPathExpr = [ "/" qualifiedComplexTypeName ]
//*                      [ collectionPathExpr ]
named!(complexColPathExpr<&str, &str>, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(collectionPathExpr))));
//*
//* collectionPathExpr = count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                    / "/" boundFunctionExpr
//*                    / "/" annotationExpr
//*                    / "/" anyExpr
//*                    / "/" allExpr
named!(collectionPathExpr<&str, &str>, alt_complete!(
					    recognize!(tuple!(count, opt!(tuple!(OPEN, expandCountOption, many0!(tuple!(SEMI, expandCountOption)), CLOSE))))
					    | recognize!(tuple!(tag!("/"), boundFunctionExpr))
					    | recognize!(tuple!(tag!("/"), annotationExpr))
					    | recognize!(tuple!(tag!("/"), anyExpr))
					    | recognize!(tuple!(tag!("/"), allExpr))
					   ));
//*
//* complexPathExpr = [ "/" qualifiedComplexTypeName ]
//*                   [ "/" propertyPathExpr
//*                   / "/" boundFunctionExpr
//*                   / "/" annotationExpr
//*                   ]
named!(complexPathExpr<&str, &str>, recognize!(tuple!(
						      opt!(tuple!(tag!("/"), qualifiedComplexTypeName)),
						      opt!(alt_complete!(
								tuple!(tag!("/"), propertyPathExpr)
								| tuple!(tag!("/"), boundFunctionExpr)
								| tuple!(tag!("/"), annotationExpr)
						      ))
						)));
//*
//* primitivePathExpr = "/" [ annotationExpr / boundFunctionExpr ]
named!(primitivePathExpr<&str, &str>, recognize!(tuple!(tag!("/"), opt!(alt_complete!(annotationExpr | boundFunctionExpr)))));
//*
//* boundFunctionExpr = functionExpr ; boundFunction segments can only be composed if the type of the
//*                                  ; previous segment matches the type of the first function parameter
//TODO(validation)
named!(boundFunctionExpr<&str, &str>, recognize!(functionExpr));
//*
//* functionExpr = namespace "."
//*                ( entityColFunction    functionExprParameters [ collectionNavigationExpr ]
//*                / entityFunction       functionExprParameters [ singleNavigationExpr ]
//*                / complexColFunction   functionExprParameters [ complexColPathExpr ]
//*                / complexFunction      functionExprParameters [ complexPathExpr ]
//*                / primitiveColFunction functionExprParameters [ collectionPathExpr ]
//*                / primitiveFunction    functionExprParameters [ primitivePathExpr ]
//*                )
named!(functionExpr<&str, &str>, recognize!(tuple!(namespace,
						   tag!("."),
						   alt_complete!(
							recognize!(tuple!(entityColFunction, functionExprParameters, opt!(collectionNavigationExpr)))
							| recognize!(tuple!(entityFunction, functionExprParameters, opt!(singleNavigationExpr)))
							| recognize!(tuple!(complexColFunction, functionExprParameters, opt!(complexColPathExpr)))
							| recognize!(tuple!(complexFunction, functionExprParameters, opt!(complexPathExpr)))
							| recognize!(tuple!(primitiveColFunction, functionExprParameters, opt!(collectionPathExpr)))
							| recognize!(tuple!(primitiveFunction, functionExprParameters, opt!(primitivePathExpr)))
						   ))));
//*
//* functionExprParameters = OPEN [ functionExprParameter *( COMMA functionExprParameter ) ] CLOSE
named!(functionExprParameters<&str, &str>, recognize!(tuple!(OPEN, opt!(tuple!(functionExprParameter, many0!(tuple!(COMMA, functionExprParameter)))), CLOSE)));
//* functionExprParameter  = parameterName EQ ( parameterAlias / parameterValue )
named!(functionExprParameter<&str, &str>, recognize!(tuple!(parameterName, EQ, alt_complete!(parameterAlias | parameterValue))));
//*
//* anyExpr = "any" OPEN BWS [ lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr ] BWS CLOSE
named!(anyExpr<&str, &str>, recognize!(tuple!(tag_no_case!("any"), OPEN, BWS, opt!(tuple!(lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr)), BWS, CLOSE)));
//* allExpr = "all" OPEN BWS   lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr   BWS CLOSE
named!(allExpr<&str, &str>, recognize!(tuple!(tag_no_case!("all"), OPEN, BWS, lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr, BWS, CLOSE)));
//* lambdaPredicateExpr = boolCommonExpr ; containing at least one lambdaVariableExpr
//TODO(use verify!() to verify that it contains at least one lambdaVariableExpr)
named!(lambdaPredicateExpr<&str, &str>, recognize!(boolCommonExpr));
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
named!(methodCallExpr<&str, &str>, alt_complete!(indexOfMethodCallExpr
					| toLowerMethodCallExpr
					| toUpperMethodCallExpr
					| trimMethodCallExpr
					| substringMethodCallExpr
					| concatMethodCallExpr
					| lengthMethodCallExpr
					| yearMethodCallExpr
					| monthMethodCallExpr
					| dayMethodCallExpr
					| hourMethodCallExpr
					| minuteMethodCallExpr
					| secondMethodCallExpr
					| fractionalsecondsMethodCallExpr
					| totalsecondsMethodCallExpr
					| dateMethodCallExpr
					| timeMethodCallExpr
					| roundMethodCallExpr
					| floorMethodCallExpr
					| ceilingMethodCallExpr
					| distanceMethodCallExpr
					| geoLengthMethodCallExpr
					| totalOffsetMinutesMethodCallExpr
					| minDateTimeMethodCallExpr
					| maxDateTimeMethodCallExpr
					| nowMethodCallExpr
					| boolMethodCallExpr));
//*
//* boolMethodCallExpr = endsWithMethodCallExpr
//*                    / startsWithMethodCallExpr
//*                    / containsMethodCallExpr
//*                    / intersectsMethodCallExpr
//*                    / hasSubsetMethodCallExpr
//*                    / hasSubsequenceMethodCallExpr
named!(boolMethodCallExpr<&str, &str>, alt_complete!(endsWithMethodCallExpr
					    | startsWithMethodCallExpr
					    | containsMethodCallExpr
					    | intersectsMethodCallExpr
					    | hasSubsetMethodCallExpr
					    | hasSubsequenceMethodCallExpr));
//*
//* concatMethodCallExpr     = "concat"     OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(concatMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("concat"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* containsMethodCallExpr   = "contains"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(containsMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("contains"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* endsWithMethodCallExpr   = "endswith"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(endsWithMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("endswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* indexOfMethodCallExpr    = "indexof"    OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(indexOfMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("indexof"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* lengthMethodCallExpr     = "length"     OPEN BWS commonExpr BWS CLOSE
named!(lengthMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("length"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* startsWithMethodCallExpr = "startswith" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(startsWithMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("startswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* substringMethodCallExpr  = "substring"  OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS [ COMMA BWS commonExpr BWS ] CLOSE
named!(substringMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("substring"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, opt!(tuple!(COMMA, BWS, commonExpr, BWS)), CLOSE)));
//* toLowerMethodCallExpr    = "tolower"    OPEN BWS commonExpr BWS CLOSE
named!(toLowerMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("tolower"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* toUpperMethodCallExpr    = "toupper"    OPEN BWS commonExpr BWS CLOSE
named!(toUpperMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("toupper"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* trimMethodCallExpr       = "trim"       OPEN BWS commonExpr BWS CLOSE
named!(trimMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("trim"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//*
//* yearMethodCallExpr               = "year"               OPEN BWS commonExpr BWS CLOSE
named!(yearMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("year"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* monthMethodCallExpr              = "month"              OPEN BWS commonExpr BWS CLOSE
named!(monthMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("month"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* dayMethodCallExpr                = "day"                OPEN BWS commonExpr BWS CLOSE
named!(dayMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("day"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* hourMethodCallExpr               = "hour"               OPEN BWS commonExpr BWS CLOSE
named!(hourMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("hour"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* minuteMethodCallExpr             = "minute"             OPEN BWS commonExpr BWS CLOSE
named!(minuteMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("minute"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* secondMethodCallExpr             = "second"             OPEN BWS commonExpr BWS CLOSE
named!(secondMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("second"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* fractionalsecondsMethodCallExpr  = "fractionalseconds"  OPEN BWS commonExpr BWS CLOSE
named!(fractionalsecondsMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("fractionalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* totalsecondsMethodCallExpr       = "totalseconds"       OPEN BWS commonExpr BWS CLOSE
named!(totalsecondsMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("totalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* dateMethodCallExpr               = "date"               OPEN BWS commonExpr BWS CLOSE
named!(dateMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("date"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* timeMethodCallExpr               = "time"               OPEN BWS commonExpr BWS CLOSE
named!(timeMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("time"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* totalOffsetMinutesMethodCallExpr = "totaloffsetminutes" OPEN BWS commonExpr BWS CLOSE
named!(totalOffsetMinutesMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("totaloffsetminutes"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//*
//* minDateTimeMethodCallExpr = "mindatetime" OPEN BWS CLOSE
named!(minDateTimeMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("mindatetime"), OPEN, BWS, CLOSE)));
//* maxDateTimeMethodCallExpr = "maxdatetime" OPEN BWS CLOSE
named!(maxDateTimeMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("maxdatetime"), OPEN, BWS, CLOSE)));
//* nowMethodCallExpr         = "now"         OPEN BWS CLOSE
named!(nowMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("now"), OPEN, BWS, CLOSE)));
//*
//* roundMethodCallExpr   = "round"   OPEN BWS commonExpr BWS CLOSE
named!(roundMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("round"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* floorMethodCallExpr   = "floor"   OPEN BWS commonExpr BWS CLOSE
named!(floorMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("floor"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* ceilingMethodCallExpr = "ceiling" OPEN BWS commonExpr BWS CLOSE
named!(ceilingMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("ceiling"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//*
//* distanceMethodCallExpr   = "geo.distance"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(distanceMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("geo.distance"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* geoLengthMethodCallExpr  = "geo.length"     OPEN BWS commonExpr BWS CLOSE
named!(geoLengthMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("geo.length"), OPEN, BWS, commonExpr, BWS, CLOSE)));
//* intersectsMethodCallExpr = "geo.intersects" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(intersectsMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("geo.intersects"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//*
//* hasSubsetMethodCallExpr      = "hassubset"      OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsetMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("hassubset"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//* hasSubsequenceMethodCallExpr = "hassubsequence" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsequenceMethodCallExpr<&str, &str>, recognize!(tuple!(tag_no_case!("hassubsequence"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE)));
//*
//* parenExpr = OPEN BWS commonExpr BWS CLOSE
named!(parenExpr<&str, &str>, recognize!(tuple!(OPEN, BWS, commonExpr, BWS, CLOSE)));
//* listExpr  = OPEN BWS commonExpr BWS *( COMMA BWS commonExpr BWS ) CLOSE
named!(listExpr<&str, &str>, recognize!(tuple!(OPEN, BWS, commonExpr, many0!(tuple!(COMMA, BWS, commonExpr, BWS)), CLOSE)));
//*
//* andExpr = RWS "and" RWS boolCommonExpr
named!(andExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("and"), RWS, boolCommonExpr)));
//* orExpr  = RWS "or"  RWS boolCommonExpr
named!(orExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("or"), RWS, boolCommonExpr)));
//*
//* eqExpr = RWS "eq" RWS commonExpr
named!(eqExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("eq"), RWS, commonExpr)));
//* neExpr = RWS "ne" RWS commonExpr
named!(neExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("ne"), RWS, commonExpr)));
//* ltExpr = RWS "lt" RWS commonExpr
named!(ltExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("lt"), RWS, commonExpr)));
//* leExpr = RWS "le" RWS commonExpr
named!(leExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("le"), RWS, commonExpr)));
//* gtExpr = RWS "gt" RWS commonExpr
named!(gtExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("gt"), RWS, commonExpr)));
//* geExpr = RWS "ge" RWS commonExpr
named!(geExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("ge"), RWS, commonExpr)));
//* inExpr = RWS "in" RWS commonExpr
named!(inExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("in"), RWS, commonExpr)));
//*
//* hasExpr = RWS "has" RWS enum
named!(hasExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("has"), RWS, commonExpr)));
//*
//* addExpr   = RWS "add"   RWS commonExpr
named!(addExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("add"), RWS, commonExpr)));
//* subExpr   = RWS "sub"   RWS commonExpr
named!(subExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("sub"), RWS, commonExpr)));
//* mulExpr   = RWS "mul"   RWS commonExpr
named!(mulExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("mul"), RWS, commonExpr)));
//* divExpr   = RWS "div"   RWS commonExpr
named!(divExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("div"), RWS, commonExpr)));
//* divbyExpr = RWS "divby" RWS commonExpr
named!(divbyExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("divby"), RWS, commonExpr)));
//* modExpr   = RWS "mod"   RWS commonExpr
named!(modExpr<&str, &str>, recognize!(tuple!(RWS, tag_no_case!("mod"), RWS, commonExpr)));
//*
//* negateExpr = "-" BWS commonExpr
named!(negateExpr<&str, &str>, recognize!(tuple!(tag!("-"), BWS, commonExpr)));
//*
//* notExpr = "not" RWS boolCommonExpr
named!(notExpr<&str, &str>, recognize!(tuple!(tag_no_case!("not"), RWS, boolCommonExpr)));
//*
//* isofExpr = "isof" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(isofExpr<&str, &str>, recognize!(tuple!(tag_no_case!("isof"), OPEN, BWS, opt!(tuple!(commonExpr, BWS, COMMA, BWS)), qualifiedTypeName, BWS, CLOSE)));
//* castExpr = "cast" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(castExpr<&str, &str>, recognize!(tuple!(tag_no_case!("cast"), OPEN, BWS, opt!(tuple!(commonExpr, BWS, COMMA, BWS)), qualifiedTypeName, BWS, CLOSE)));
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
named!(arrayOrObject<&str, &str>, alt_complete!(complexColInUri | complexInUri | rootExprCol | primitiveColInUri));
//*
//* complexColInUri = begin-array
//*                   [ complexInUri *( value-separator complexInUri ) ]
//*                   end-array
named!(complexColInUri<&str, &str>, recognize!(tuple!(begin_array, opt!(tuple!(complexInUri, many0!(tuple!(value_separator, complexInUri)))), end_array)));
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
named!(complexInUri<&str, &str>, recognize!(tuple!(begin_object,
						   opt!(tuple!(
							       alt_complete!(  annotationInUri
								    | primitivePropertyInUri
								    | complexPropertyInUri
								    | collectionPropertyInUri
								    | navigationPropertyInUri),
								many0!(tuple!(
									      value_separator,
									      alt_complete!(  annotationInUri
										   | primitivePropertyInUri
										   | complexPropertyInUri
										   | collectionPropertyInUri
										   | navigationPropertyInUri)
								))
						    )),
						    end_object
						)));

//*
//* collectionPropertyInUri = ( quotation-mark primitiveColProperty quotation-mark
//*                             name-separator
//*                             primitiveColInUri
//*                           )
//*                         / ( quotation-mark complexColProperty quotation-mark
//*                             name-separator
//*                             complexColInUri
//*                           )
named!(collectionPropertyInUri<&str, &str>, alt_complete!(recognize!(tuple!(quotation_mark, primitiveColProperty, quotation_mark, name_separator, primitiveColInUri))
						 | recognize!(tuple!(quotation_mark, complexColProperty, quotation_mark, name_separator, complexColInUri))));

//*
//* primitiveColInUri = begin-array
//*                     [ primitiveLiteralInJSON *( value-separator primitiveLiteralInJSON ) ]
//*                     end-array
named!(primitiveColInUri<&str, &str>, recognize!(tuple!(begin_array, opt!(tuple!(primitiveLiteralInJSON, many0!(tuple!(value_separator, primitiveLiteralInJSON)))), end_array)));
//*
//* complexPropertyInUri = quotation-mark complexProperty quotation-mark
//*                        name-separator
//*                        complexInUri
named!(complexPropertyInUri<&str, &str>, recognize!(tuple!(quotation_mark, complexProperty, quotation_mark, name_separator, complexInUri)));
//*
//* annotationInUri = quotation-mark AT namespace "." termName quotation-mark
//*                   name-separator
//*                   ( complexInUri / complexColInUri / primitiveLiteralInJSON / primitiveColInUri )
named!(annotationInUri<&str, &str>, recognize!(tuple!(quotation_mark, AT, namespace, tag!("."), termName, quotation_mark,
						      name_separator,
						      alt_complete!(complexInUri | complexColInUri | primitiveLiteralInJSON | primitiveColInUri))));
//*
//* primitivePropertyInUri = quotation-mark primitiveProperty quotation-mark
//*                          name-separator
//*                          primitiveLiteralInJSON
named!(primitivePropertyInUri<&str, &str>, recognize!(tuple!(quotation_mark, primitiveProperty, quotation_mark, name_separator, primitiveLiteralInJSON)));
//*
//* navigationPropertyInUri = singleNavPropInJSON
//*                         / collectionNavPropInJSON
named!(navigationPropertyInUri<&str, &str>, alt_complete!(singleNavPropInJSON | collectionNavPropInJSON));
//* singleNavPropInJSON     = quotation-mark entityNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExpr
named!(singleNavPropInJSON<&str, &str>, recognize!(tuple!(quotation_mark, entityNavigationProperty, quotation_mark, name_separator, rootExpr)));
//* collectionNavPropInJSON = quotation-mark entityColNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExprCol
named!(collectionNavPropInJSON<&str, &str>, recognize!(tuple!(quotation_mark, entityColNavigationProperty, quotation_mark, name_separator, rootExprCol)));
//*
//* rootExprCol = begin-array
//*               [ rootExpr *( value-separator rootExpr ) ]
//*               end-array
named!(rootExprCol<&str, &str>, recognize!(tuple!(begin_array, opt!(tuple!(rootExpr, many0!(tuple!(value_separator, rootExpr)))), end_array)));
//*
//* ; JSON syntax: adapted to URI restrictions from [RFC4627]
//* begin-object = BWS ( "{" / "%7B" ) BWS
named!(begin_object<&str, &str>, recognize!(tuple!(BWS, alt_complete!(tag!("{") | tag!("%7B")))));
//* end-object   = BWS ( "}" / "%7D" )
named!(end_object<&str, &str>, recognize!(tuple!(BWS, alt_complete!(tag!("}") | tag!("%7D")))));
//*
//* begin-array = BWS ( "[" / "%5B" ) BWS
named!(begin_array<&str, &str>, recognize!(tuple!(BWS, alt_complete!(tag!("[") | tag!("%5B")))));
//* end-array   = BWS ( "]" / "%5D" )
named!(end_array<&str, &str>, recognize!(tuple!(BWS, alt_complete!(tag!("]") | tag!("%5D")))));
//*
//* quotation-mark  = DQUOTE / "%22"
named!(quotation_mark<&str, &str>, alt_complete!(recognize!(DQUOTE) | tag!("%22")));
//* name-separator  = BWS COLON BWS
named!(name_separator<&str, &str>, recognize!(tuple!(BWS, COLON, BWS)));
//* value-separator = BWS COMMA BWS
named!(value_separator<&str, &str>, recognize!(tuple!(BWS, COMMA, BWS)));
//*
//* primitiveLiteralInJSON = stringInJSON
//*                        / numberInJSON
//*                        / 'true'
//*                        / 'false'
//*                        / 'null'
named!(primitiveLiteralInJSON<&str, &str>, alt_complete!(stringInJSON | numberInJSON | tag!("true") | tag!("false") | tag!("null")));
//*
//* stringInJSON = quotation-mark *charInJSON quotation-mark
named!(stringInJSON<&str, &str>, recognize!(tuple!(quotation_mark, many0!(charInJSON), quotation_mark)));
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
named!(charInJSON<&str, &str>, alt_complete!(qchar_unescaped | qchar_JSON_special | recognize!(tuple!(escape, alt_complete!(  quotation_mark
													  | escape
													  | alt_complete!(tag!("/") | tag!("%2F"))
													  | recognize!(one_of!("bfnrt"))
													  | recognize!(tuple!(tag!("u"), many_m_n!(4, 4, HEXDIG)))
													  )))));
//*
//* qchar-JSON-special = SP / ":" / "{" / "}" / "[" / "]" ; some agents put these unencoded into the query part of a URL
named!(qchar_JSON_special<&str, &str>, alt_complete!(SP | recognize!(one_of!(":{}[]"))));
//*
//* escape = "\" / "%5C"     ; reverse solidus U+005C
named!(escape<&str, &str>, alt_complete!(tag!("\\") | tag!("%5C")));
//*
//* numberInJSON = [ "-" ] int [ frac ] [ exp ]
named!(numberInJSON<&str, &str>, recognize!(tuple!(opt!(tag!("-")), int, opt!(frac), opt!(exp))));
//* int          = "0" / ( oneToNine *DIGIT )
named!(int<&str, &str>, alt_complete!(tag!("0") | recognize!(tuple!(oneToNine, many0!(DIGIT)))));
//* frac         = "." 1*DIGIT
named!(frac<&str, &str>, recognize!(tuple!(tag!("."), many1!(DIGIT))));
//* exp          = "e" [ "-" / "+" ] 1*DIGIT
named!(exp<&str, &str>, recognize!(tuple!(tag_no_case!("e"), opt!(alt_complete!(tag!("-") | tag!("+"))), many1!(DIGIT))));
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
named!(singleQualifiedTypeName<&str, &str>, alt_complete!(qualifiedEntityTypeName
						 | qualifiedComplexTypeName
						 | qualifiedTypeDefinitionName
						 | qualifiedEnumTypeName
						 | primitiveTypeName));
//*
//* qualifiedTypeName = singleQualifiedTypeName
//*                   / 'Collection' OPEN singleQualifiedTypeName CLOSE
named!(qualifiedTypeName<&str, &str>, alt_complete!(singleQualifiedTypeName
					   | recognize!(tuple!(tag!("Collection"), OPEN, singleQualifiedTypeName, CLOSE))));
//*
//* qualifiedEntityTypeName     = namespace "." entityTypeName
named!(qualifiedEntityTypeName<&str, &str>, recognize!(tuple!(namespace, tag!("."), entityTypeName)));
//* qualifiedComplexTypeName    = namespace "." complexTypeName
named!(qualifiedComplexTypeName<&str, &str>, recognize!(tuple!(namespace, tag!("."), complexTypeName)));
//* qualifiedTypeDefinitionName = namespace "." typeDefinitionName
named!(qualifiedTypeDefinitionName<&str, &str>, recognize!(tuple!(namespace, tag!("."), typeDefinitionName)));
//* qualifiedEnumTypeName       = namespace "." enumerationTypeName
named!(qualifiedEnumTypeName<&str, &str>, recognize!(tuple!(namespace, tag!("."), enumerationTypeName)));
//*
//* ; an alias is just a single-part namespace
//* namespace     = namespacePart *( "." namespacePart )
named!(namespace<&str, &str>, recognize!(tuple!(namespacePart, many0!(tuple!(tag!("."), namespacePart)))));
//* namespacePart = odataIdentifier
named!(namespacePart<&str, &str>, recognize!(odataIdentifier));
//*
//* entitySetName       = odataIdentifier
named!(entitySetName<&str, &str>, recognize!(odataIdentifier));
//* singletonEntity     = odataIdentifier
named!(singletonEntity<&str, &str>, recognize!(odataIdentifier));
//* entityTypeName      = odataIdentifier
named!(entityTypeName<&str, &str>, recognize!(odataIdentifier));
//* complexTypeName     = odataIdentifier
named!(complexTypeName<&str, &str>, recognize!(odataIdentifier));
//* typeDefinitionName  = odataIdentifier
named!(typeDefinitionName<&str, &str>, recognize!(odataIdentifier));
//* enumerationTypeName = odataIdentifier
named!(enumerationTypeName<&str, &str>, recognize!(odataIdentifier));
//* enumerationMember   = odataIdentifier
named!(enumerationMember<&str, &str>, recognize!(odataIdentifier));
//* termName            = odataIdentifier
named!(termName<&str, &str>, recognize!(odataIdentifier));

//TODO(restrictive + unicode)
//* ; Note: this pattern is overly restrictive, the normative definition is type TSimpleIdentifier in OData EDM XML Schema
//* odataIdentifier             = identifierLeadingCharacter *127identifierCharacter
named!(odataIdentifier<&str, &str>, recognize!(tuple!(identifierLeadingCharacter, many_m_n!(0, 127, identifierCharacter))));
//* identifierLeadingCharacter  = ALPHA / "_"         ; plus Unicode characters from the categories L or Nl
named!(identifierLeadingCharacter<&str, &str>, alt_complete!(ALPHA | tag!("_")));
//* identifierCharacter         = ALPHA / "_" / DIGIT ; plus Unicode characters from the categories L, Nl, Nd, Mn, Mc, Pc, or Cf
named!(identifierCharacter<&str, &str>, alt_complete!(ALPHA | tag!("_") | DIGIT));
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
named!(primitiveTypeName<&str, &str>, recognize!(tuple!(tag!("Edm."), alt_complete!(  tag!("Binary")
									   | tag!("Boolean")
									   | tag!("Byte")
									   | tag!("Date")
									   | tag!("DateTimeOffset")
									   | tag!("Decimal")
									   | tag!("Double")
									   | tag!("Duration")
									   | tag!("Guid")
									   | tag!("Int16")
									   | tag!("Int32")
									   | tag!("Int64")
									   | tag!("SByte")
									   | tag!("Single")
									   | tag!("Stream")
									   | tag!("String")
									   | tag!("TimeOfDay")
									   | recognize!(tuple!(abstractSpatialTypeName, opt!(concreteSpatialTypeName)))
									))));
//* abstractSpatialTypeName = 'Geography'
//*                         / 'Geometry'
named!(abstractSpatialTypeName<&str, &str>, alt_complete!(tag!("Geography") | tag!("Geometry")));
//* concreteSpatialTypeName = 'Collection'
//*                         / 'LineString'
//*                         / 'MultiLineString'
//*                         / 'MultiPoint'
//*                         / 'MultiPolygon'
//*                         / 'Point'
//*                         / 'Polygon'
named!(concreteSpatialTypeName<&str, &str>, alt_complete!(  tag!("Collection")
						 | tag!("LineString")
						 | tag!("MultiLineString")
						 | tag!("MultiPoint")
						 | tag!("MultiPolygon")
						 | tag!("Point")
						 | tag!("Polygon")
						 ));
//*
//* primitiveProperty       = primitiveKeyProperty / primitiveNonKeyProperty
named!(primitiveProperty<&str, &str>, alt_complete!(primitiveKeyProperty | primitiveNonKeyProperty));
//* primitiveKeyProperty    = odataIdentifier
named!(primitiveKeyProperty<&str, &str>, recognize!(odataIdentifier));
//* primitiveNonKeyProperty = odataIdentifier
named!(primitiveNonKeyProperty<&str, &str>, recognize!(odataIdentifier));
//* primitiveColProperty    = odataIdentifier
named!(primitiveColProperty<&str, &str>, recognize!(odataIdentifier));
//* complexProperty         = odataIdentifier
named!(complexProperty<&str, &str>, recognize!(odataIdentifier));
//* complexColProperty      = odataIdentifier
named!(complexColProperty<&str, &str>, recognize!(odataIdentifier));
//* streamProperty          = odataIdentifier
named!(streamProperty<&str, &str>, recognize!(odataIdentifier));
//*
//* navigationProperty          = entityNavigationProperty / entityColNavigationProperty
named!(navigationProperty<&str, &str>, alt_complete!(entityNavigationProperty | entityColNavigationProperty));
//* entityNavigationProperty    = odataIdentifier
named!(entityNavigationProperty<&str, &str>, recognize!(odataIdentifier));
//* entityColNavigationProperty = odataIdentifier
named!(entityColNavigationProperty<&str, &str>, recognize!(odataIdentifier));
//*
//* action       = odataIdentifier
named!(action<&str, &str>, recognize!(odataIdentifier));
//* actionImport = odataIdentifier
named!(actionImport<&str, &str>, recognize!(odataIdentifier));
//*
//* function = entityFunction
//*          / entityColFunction
//*          / complexFunction
//*          / complexColFunction
//*          / primitiveFunction
//*          / primitiveColFunction
named!(function<&str, &str>, alt_complete!( entityFunction
				 | entityColFunction
				 | complexFunction
				 | complexColFunction
				 | primitiveFunction
				 | primitiveColFunction));
//*
//* entityFunction       = odataIdentifier
named!(entityFunction<&str, &str>, recognize!(odataIdentifier));
//* entityColFunction    = odataIdentifier
named!(entityColFunction<&str, &str>, recognize!(odataIdentifier));
//* complexFunction      = odataIdentifier
named!(complexFunction<&str, &str>, recognize!(odataIdentifier));
//* complexColFunction   = odataIdentifier
named!(complexColFunction<&str, &str>, recognize!(odataIdentifier));
//* primitiveFunction    = odataIdentifier
named!(primitiveFunction<&str, &str>, recognize!(odataIdentifier));
//* primitiveColFunction = odataIdentifier
named!(primitiveColFunction<&str, &str>, recognize!(odataIdentifier));
//*
//* entityFunctionImport       = odataIdentifier
named!(entityFunctionImport<&str, &str>, recognize!(odataIdentifier));
//* entityColFunctionImport    = odataIdentifier
named!(entityColFunctionImport<&str, &str>, recognize!(odataIdentifier));
//* complexFunctionImport      = odataIdentifier
named!(complexFunctionImport<&str, &str>, recognize!(odataIdentifier));
//* complexColFunctionImport   = odataIdentifier
named!(complexColFunctionImport<&str, &str>, recognize!(odataIdentifier));
//* primitiveFunctionImport    = odataIdentifier
named!(primitiveFunctionImport<&str, &str>, recognize!(odataIdentifier));
//* primitiveColFunctionImport = odataIdentifier
named!(primitiveColFunctionImport<&str, &str>, recognize!(odataIdentifier));
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
named!(primitiveLiteral<&str, &str>, alt_complete!(  nullValue
					  | booleanValue
					  | guidValue
					  | dateValue
					  | dateTimeOffsetValue
					  | timeOfDayValue
					  | decimalValue
					  | doubleValue
					  | singleValue
					  | sbyteValue
					  | byteValue
					  | int16Value
					  | int32Value
					  | int64Value
					  | string
					  | duration
					  | _enum
					  | binary
					  | geographyCollection
					  | geographyLineString
					  | geographyMultiLineString
					  | geographyMultiPoint
					  | geographyMultiPolygon
					  | geographyPoint
					  | geographyPolygon
					  | geometryCollection
					  | geometryLineString
					  | geometryMultiLineString
					  | geometryMultiPoint
					  | geometryMultiPolygon
					  | geometryPoint
					  | geometryPolygon));
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
named!(primitiveValue<&str, &str>, alt_complete!( booleanValue
				       | guidValue
				       | durationValue
				       | dateValue
				       | dateTimeOffsetValue
				       | timeOfDayValue
				       | enumValue
				       | fullCollectionLiteral
				       | fullLineStringLiteral
				       | fullMultiPointLiteral
				       | fullMultiLineStringLiteral
				       | fullMultiPolygonLiteral
				       | fullPointLiteral
				       | fullPolygonLiteral
				       | decimalValue
				       | doubleValue
				       | singleValue
				       | sbyteValue
				       | byteValue
				       | int16Value
				       | int32Value
				       | int64Value
				       | binaryValue));
//*
//* nullValue = 'null'
named!(nullValue<&str, &str>, tag!("null"));
//*
//* ; base64url encoding according to http://tools.ietf.org/html/rfc4648#section-5
//* binary      = "binary" SQUOTE binaryValue SQUOTE
named!(binary<&str, &str>, recognize!(tuple!(tag_no_case!("binary"), SQUOTE, binaryValue, SQUOTE)));
//* binaryValue = *(4base64char) [ base64b16  / base64b8 ]
named!(binaryValue<&str, &str>, recognize!(tuple!(many0!(many_m_n!(4, 4, base64char)), opt!(alt_complete!(base64b16 | base64b8)))));
//* base64b16   = 2base64char ( 'A' / 'E' / 'I' / 'M' / 'Q' / 'U' / 'Y' / 'c' / 'g' / 'k' / 'o' / 's' / 'w' / '0' / '4' / '8' )   [ "=" ]
named!(base64b16<&str, &str>, recognize!(tuple!(many_m_n!(2, 2, base64char), one_of!("AEIMQUYcgkosw048"), opt!(tag!("=")))));
//* base64b8    = base64char ( 'A' / 'Q' / 'g' / 'w' ) [ "==" ]
named!(base64b8<&str, &str>, recognize!(tuple!(base64char, one_of!("AQgw"), opt!(tag!("==")))));
//* base64char  = ALPHA / DIGIT / "-" / "_"
named!(base64char<&str, &str>, alt_complete!(ALPHA | DIGIT | tag!("-") | tag!("_")));
//*
//* booleanValue = "true" / "false"
named!(booleanValue<&str, &str>, alt_complete!(tag_no_case!("true") | tag_no_case!("false")));
//*
//* decimalValue = [ SIGN ] 1*DIGIT [ "." 1*DIGIT ] [ "e" [ SIGN ] 1*DIGIT ] / nanInfinity
named!(decimalValue<&str, &str>, alt_complete!(recognize!(tuple!(opt!(SIGN),
							many1!(DIGIT),
						   	opt!(tuple!(tag!("."), many1!(DIGIT))),
						   	opt!(tuple!(tag_no_case!("e"), opt!(SIGN), many1!(DIGIT)))
							))
				      | nanInfinity));
//* doubleValue  = decimalValue ; IEEE 754 binary64 floating-point number (15-17 decimal digits)
named!(doubleValue<&str, &str>, recognize!(decimalValue));
//* singleValue  = decimalValue ; IEEE 754 binary32 floating-point number (6-9 decimal digits)
named!(singleValue<&str, &str>, recognize!(decimalValue));
//* nanInfinity  = 'NaN' / '-INF' / 'INF'
named!(nanInfinity<&str, &str>, alt_complete!(tag!("NaN") | tag!("-INF") | tag!("INF")));
//*
//* guidValue = 8HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 12HEXDIG
named!(guidValue<&str, &str>, recognize!(tuple!(many_m_n!(8, 8, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(12, 12, HEXDIG)
						)));
//*
//* byteValue  = 1*3DIGIT           ; numbers in the range from 0 to 255
named!(byteValue<&str, &str>, recognize!(many_m_n!(1, 3, DIGIT)));
//* sbyteValue = [ SIGN ] 1*3DIGIT  ; numbers in the range from -128 to 127
named!(sbyteValue<&str, &str>, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 3, DIGIT))));
//* int16Value = [ SIGN ] 1*5DIGIT  ; numbers in the range from -32768 to 32767
named!(int16Value<&str, &str>, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 5, DIGIT))));
//* int32Value = [ SIGN ] 1*10DIGIT ; numbers in the range from -2147483648 to 2147483647
named!(int32Value<&str, &str>, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 10, DIGIT))));
//* int64Value = [ SIGN ] 1*19DIGIT ; numbers in the range from -9223372036854775808 to 9223372036854775807
named!(int64Value<&str, &str>, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 19, DIGIT))));
//*
//* string           = SQUOTE *( SQUOTE-in-string / pchar-no-SQUOTE ) SQUOTE
named!(string<&str, &str>, recognize!(tuple!(SQUOTE, many0!(alt_complete!(recognize!(SQUOTE_in_string) | recognize!(pchar_no_SQUOTE))), SQUOTE)));
//* SQUOTE-in-string = SQUOTE SQUOTE ; two consecutive single quotes represent one within a string literal
named!(SQUOTE_in_string<&str, &str>, recognize!(tuple!(SQUOTE, SQUOTE)));
//*
//* dateValue = year "-" month "-" day
named!(dateValue<&str, &str>, recognize!(tuple!(year, tag!("-"), month, tag!("-"), day)));
//*
//* dateTimeOffsetValue = year "-" month "-" day "T" hour ":" minute [ ":" second [ "." fractionalSeconds ] ] ( "Z" / SIGN hour ":" minute )
named!(dateTimeOffsetValue<&str, &str>, recognize!(tuple!(year, tag!("-"), month, tag!("-"), day, tag_no_case!("T"), hour, tag!(":"), minute,
							  opt!(tuple!(tag!(":"), second, opt!(tuple!(tag!("."), fractionalSeconds)))),
							  alt_complete!(tag_no_case!("Z") | recognize!(tuple!(SIGN, hour, tag!(":"), minute)))
							  )));
//*
//* duration      = [ "duration" ] SQUOTE durationValue SQUOTE
named!(duration<&str, &str>, recognize!(tuple!(opt!(tag_no_case!("duration")), SQUOTE, durationValue, SQUOTE)));
//* durationValue = [ SIGN ] "P" [ 1*DIGIT "D" ] [ "T" [ 1*DIGIT "H" ] [ 1*DIGIT "M" ] [ 1*DIGIT [ "." 1*DIGIT ] "S" ] ]
//*      ; the above is an approximation of the rules for an xml dayTimeDuration.
//*      ; see the lexical representation for dayTimeDuration in http://www.w3.org/TR/xmlschema11-2#dayTimeDuration for more information
named!(durationValue<&str, &str>, recognize!(tuple!(opt!(SIGN),
						    tag_no_case!("P"),
						    opt!(tuple!(many1!(DIGIT), tag_no_case!("D"))),
						    opt!(tuple!(tag_no_case!("T"),
								opt!(tuple!(many1!(DIGIT), tag_no_case!("H"))),
								opt!(tuple!(many1!(DIGIT), tag_no_case!("M"))),
								opt!(tuple!(many1!(DIGIT), opt!(tuple!(tag!("."), many1!(DIGIT))), tag_no_case!("S")))
								)))));

//*
//* timeOfDayValue = hour ":" minute [ ":" second [ "." fractionalSeconds ] ]
named!(timeOfDayValue<&str, &str>, recognize!(tuple!(hour, tag!(":"), minute, opt!(tuple!(tag!(":"), second, opt!(tuple!(tag!("."), fractionalSeconds)))))));
//*
//* oneToNine       = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
named!(oneToNine<&str, &str>, recognize!(one_of!("123456789")));
//* zeroToFiftyNine = ( "0" / "1" / "2" / "3" / "4" / "5" ) DIGIT
named!(zeroToFiftyNine<&str, &str>, recognize!(tuple!(one_of!("012345"), DIGIT)));
//* year  = [ "-" ] ( "0" 3DIGIT / oneToNine 3*DIGIT )
named!(year<&str, &str>, recognize!(tuple!(opt!(tag!("-")), alt_complete!(recognize!(tuple!(tag!("0"), many_m_n!(3, 3, DIGIT))) | recognize!(tuple!(oneToNine, many_m_n!(3, 3, DIGIT)))))));
//* month = "0" oneToNine
//*       / "1" ( "0" / "1" / "2" )
named!(month<&str, &str>, alt_complete!(  recognize!(tuple!(tag!("0"), oneToNine))
			       | recognize!(tuple!(tag!("1"), one_of!("012")))
			       ));
//* day   = "0" oneToNine
//*       / ( "1" / "2" ) DIGIT
//*       / "3" ( "0" / "1" )
named!(day<&str, &str>, alt_complete!(  recognize!(tuple!(tag!("0"), oneToNine))
			     | recognize!(tuple!(one_of!("12"), DIGIT))
			     | recognize!(tuple!(tag!("3"), one_of!("01")))
			     ));
//* hour   = ( "0" / "1" ) DIGIT
//*        / "2" ( "0" / "1" / "2" / "3" )
named!(hour<&str, &str>, alt_complete!(  recognize!(tuple!(one_of!("01"), DIGIT))
			      | recognize!(tuple!(tag!("2"), one_of!("0123")))
			      ));
//* minute = zeroToFiftyNine
named!(minute<&str, &str>, recognize!(zeroToFiftyNine));
//* second = zeroToFiftyNine
named!(second<&str, &str>, recognize!(zeroToFiftyNine));
//* fractionalSeconds = 1*12DIGIT
named!(fractionalSeconds<&str, &str>, recognize!(many_m_n!(1, 12, DIGIT)));
//*
//* enum            = [ qualifiedEnumTypeName ] SQUOTE enumValue SQUOTE
named!(_enum<&str, &str>, recognize!(tuple!(opt!(qualifiedEntityTypeName), SQUOTE, enumValue, SQUOTE)));
//* enumValue       = singleEnumValue *( COMMA singleEnumValue )
named!(enumValue<&str, &str>, recognize!(tuple!(singleEnumValue, many0!(tuple!(COMMA, singleEnumValue)))));
//* singleEnumValue = enumerationMember / enumMemberValue
named!(singleEnumValue<&str, &str>, alt_complete!(enumerationMember | enumMemberValue));
//* enumMemberValue = int64Value
named!(enumMemberValue<&str, &str>, recognize!(int64Value));


//* geographyCollection   = geographyPrefix SQUOTE fullCollectionLiteral SQUOTE
named!(geographyCollection<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)));
//* fullCollectionLiteral = sridLiteral collectionLiteral
named!(fullCollectionLiteral<&str, &str>, recognize!(tuple!(sridLiteral, collectionLiteral)));
//* collectionLiteral     = "Collection(" geoLiteral *( COMMA geoLiteral ) CLOSE
named!(collectionLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("Collection("), geoLiteral, many0!(tuple!(COMMA, geoLiteral)), CLOSE)));
//* geoLiteral            = collectionLiteral
//*                       / lineStringLiteral
//*                       / multiPointLiteral
//*                       / multiLineStringLiteral
//*                       / multiPolygonLiteral
//*                       / pointLiteral
//*                       / polygonLiteral
named!(geoLiteral<&str, &str>, alt_complete!(  collectionLiteral
				    | lineStringLiteral
				    | multiPointLiteral
				    | multiLineStringLiteral
				    | multiPolygonLiteral
				    | pointLiteral
				    | polygonLiteral));
//*
//* geographyLineString   = geographyPrefix SQUOTE fullLineStringLiteral SQUOTE
named!(geographyLineString<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)));
//* fullLineStringLiteral = sridLiteral lineStringLiteral
named!(fullLineStringLiteral<&str, &str>, recognize!(tuple!(sridLiteral, lineStringLiteral)));
//* lineStringLiteral     = "LineString" lineStringData
named!(lineStringLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("LineString"), lineStringData)));
//* lineStringData        = OPEN positionLiteral 1*( COMMA positionLiteral ) CLOSE
named!(lineStringData<&str, &str>, recognize!(tuple!(OPEN, positionLiteral, many1!(tuple!(COMMA, positionLiteral)), CLOSE)));
//*
//* geographyMultiLineString   = geographyPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geographyMultiLineString<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)));
//* fullMultiLineStringLiteral = sridLiteral multiLineStringLiteral
named!(fullMultiLineStringLiteral<&str, &str>, recognize!(tuple!(sridLiteral, multiLineStringLiteral)));
//* multiLineStringLiteral     = "MultiLineString(" [ lineStringData *( COMMA lineStringData ) ] CLOSE
named!(multiLineStringLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("MultiLineString("), opt!(tuple!(lineStringData, many0!(tuple!(COMMA, lineStringData)))), CLOSE)));
//*
//* geographyMultiPoint   = geographyPrefix SQUOTE fullMultiPointLiteral SQUOTE
named!(geographyMultiPoint<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)));
//* fullMultiPointLiteral = sridLiteral multiPointLiteral
named!(fullMultiPointLiteral<&str, &str>, recognize!(tuple!(sridLiteral, multiPointLiteral)));
//* multiPointLiteral     = "MultiPoint(" [ pointData *( COMMA pointData ) ] CLOSE
named!(multiPointLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("MultiPoint("), opt!(tuple!(pointData, many0!(tuple!(COMMA, pointData)))), CLOSE)));
//*
//* geographyMultiPolygon   = geographyPrefix SQUOTE fullMultiPolygonLiteral SQUOTE
named!(geographyMultiPolygon<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)));
//* fullMultiPolygonLiteral = sridLiteral multiPolygonLiteral
named!(fullMultiPolygonLiteral<&str, &str>, recognize!(tuple!(sridLiteral, multiPolygonLiteral)));
//* multiPolygonLiteral     = "MultiPolygon(" [ polygonData *( COMMA polygonData ) ] CLOSE
named!(multiPolygonLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("MultiPolygon("), opt!(tuple!(polygonData, many0!(tuple!(COMMA, polygonData)))), CLOSE)));
//*
//* geographyPoint   = geographyPrefix SQUOTE fullPointLiteral SQUOTE
named!(geographyPoint<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullPointLiteral, SQUOTE)));
//* fullPointLiteral = sridLiteral pointLiteral
named!(fullPointLiteral<&str, &str>, recognize!(tuple!(sridLiteral, pointLiteral)));
//* sridLiteral      = "SRID" EQ 1*5DIGIT SEMI
named!(sridLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("SRID"), EQ, many_m_n!(1, 5, DIGIT), SEMI)));
//* pointLiteral     ="Point" pointData
named!(pointLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("Point"), pointData)));
//* pointData        = OPEN positionLiteral CLOSE
named!(pointData<&str, &str>, recognize!(tuple!(OPEN, positionLiteral, CLOSE)));
//* positionLiteral  = doubleValue SP doubleValue  ; longitude, then latitude
named!(positionLiteral<&str, &str>, recognize!(tuple!(doubleValue, SP, doubleValue)));
//*
//* geographyPolygon   = geographyPrefix SQUOTE fullPolygonLiteral SQUOTE
named!(geographyPolygon<&str, &str>, recognize!(tuple!(geographyPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)));
//* fullPolygonLiteral = sridLiteral polygonLiteral
named!(fullPolygonLiteral<&str, &str>, recognize!(tuple!(sridLiteral, polygonLiteral)));
//* polygonLiteral     = "Polygon" polygonData
named!(polygonLiteral<&str, &str>, recognize!(tuple!(tag_no_case!("Polygon"), polygonData)));
//* polygonData        = OPEN ringLiteral *( COMMA ringLiteral ) CLOSE
named!(polygonData<&str, &str>, recognize!(tuple!(OPEN, ringLiteral, many0!(tuple!(COMMA, ringLiteral)), CLOSE)));
//* ringLiteral        = OPEN positionLiteral *( COMMA positionLiteral ) CLOSE
//*                    ; Within each ringLiteral, the first and last positionLiteral elements MUST be an exact syntactic match to each other.
//*                    ; Within the polygonData, the ringLiterals MUST specify their points in appropriate winding order.
//*                    ; In order of traversal, points to the left side of the ring are interpreted as being in the polygon.
named!(ringLiteral<&str, &str>, recognize!(tuple!(OPEN, positionLiteral, many0!(tuple!(COMMA, positionLiteral)), CLOSE)));
//*
//* geometryCollection      = geometryPrefix SQUOTE fullCollectionLiteral      SQUOTE
named!(geometryCollection<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullCollectionLiteral, SQUOTE)));
//* geometryLineString      = geometryPrefix SQUOTE fullLineStringLiteral      SQUOTE
named!(geometryLineString<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullLineStringLiteral, SQUOTE)));
//* geometryMultiLineString = geometryPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geometryMultiLineString<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE)));
//* geometryMultiPoint      = geometryPrefix SQUOTE fullMultiPointLiteral      SQUOTE
named!(geometryMultiPoint<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE)));
//* geometryMultiPolygon    = geometryPrefix SQUOTE fullMultiPolygonLiteral    SQUOTE
named!(geometryMultiPolygon<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE)));
//* geometryPoint           = geometryPrefix SQUOTE fullPointLiteral           SQUOTE
named!(geometryPoint<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullPointLiteral, SQUOTE)));
//* geometryPolygon         = geometryPrefix SQUOTE fullPolygonLiteral         SQUOTE
named!(geometryPolygon<&str, &str>, recognize!(tuple!(geometryPrefix, SQUOTE, fullPolygonLiteral, SQUOTE)));
//*
//* geographyPrefix = "geography"
named!(geographyPrefix<&str, &str>, tag_no_case!("geography"));
//* geometryPrefix  = "geometry"
named!(geometryPrefix<&str, &str>, tag_no_case!("geometry"));
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
named!(obs_text<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && (chr as u8) >= 0x80)));
//* ;quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
//*
//* OWS   = *( SP / HTAB )  ; "optional" whitespace
named!(OWS<&str, &str>, recognize!(many0!(alt_complete!(SP | HTAB))));
//* BWS-h = *( SP / HTAB )  ; "bad" whitespace in header values
named!(BWS_h<&str, &str>, recognize!(many0!(alt_complete!(SP | HTAB))));
//* EQ-h  = BWS-h EQ BWS-h
named!(EQ_h<&str, &str>, recognize!(tuple!(BWS_h, EQ, BWS_h)));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 9. Punctuation
//* ;------------------------------------------------------------------------------
//*
//* RWS = 1*( SP / HTAB / "%20" / "%09" )  ; "required" whitespace
named!(RWS<&str, &str>, recognize!(many1!(alt_complete!(SP | HTAB | tag!("%20") | tag!("%09")))));
//* BWS =  *( SP / HTAB / "%20" / "%09" )  ; "bad" whitespace
named!(BWS<&str, &str>, recognize!(many0!(alt_complete!(SP | HTAB | tag!("%20") | tag!("%09")))));
//*
//* AT     = "@" / "%40"
named!(AT<&str, &str>, alt_complete!(tag!("@") | tag!("%40")));
//* COLON  = ":" / "%3A"
named!(COLON<&str, &str>, alt_complete!(tag!(":") | tag!("%3A")));
//* COMMA  = "," / "%2C"
named!(COMMA<&str, &str>, alt_complete!(tag!(",") | tag!("%2C")));
//* EQ     = "="
named!(EQ<&str, &str>, tag!("="));
//* SIGN   = "+" / "%2B" / "-"
named!(SIGN<&str, &str>, alt_complete!(tag!("+") | tag!("%3B") | tag!("-")));
//* SEMI   = ";" / "%3B"
named!(SEMI<&str, &str>, alt_complete!(tag!(";") | tag!("%3B")));
//* STAR   = "*" / "%2A"
named!(STAR<&str, &str>, alt_complete!(tag!("*") | tag!("%2A")));
//* SQUOTE = "'" / "%27"
named!(SQUOTE<&str, &str>, alt_complete!(tag!("'") | tag!("%27")));
//*
//* OPEN  = "(" / "%28"
named!(OPEN<&str, &str>, alt_complete!(tag!("(") | tag!("%28")));
//* CLOSE = ")" / "%29"
named!(CLOSE<&str, &str>, alt_complete!(tag!(")") | tag!("%29")));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; A. URI syntax [RFC3986]
//* ;------------------------------------------------------------------------------
//*
//* URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
named!(URI<&str, &str>, recognize!(tuple!(scheme, tag!(":"), hier_part, opt!(tuple!(tag!("?"), query)), opt!(tuple!(tag!("#"), fragment)))));
//* hier-part     = "//" authority path-abempty
//*               / path-absolute
//*               / path-rootless
//* ;              / path-empty
named!(hier_part<&str, &str>, recognize!(tuple!(tag!("//"), authority, alt_complete!(path_abempty | path_absolute | path_rootless | path_empty))));
//* ;URI-reference = URI / relative-ref
//* ;absolute-URI  = scheme ":" hier-part [ "?" query ]
//* ;relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
//* ;relative-part = "//" authority path-abempty
//* ;              / path-absolute
//* ;              / path-noscheme
//* ;              / path-empty
//* scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
named!(scheme<&str, &str>, recognize!(tuple!(ALPHA, many0!(alt_complete!(ALPHA | DIGIT | tag!("+") | tag!("-") | tag!("."))))));
//* authority     = [ userinfo "@" ] host [ ":" port ]
named!(authority<&str, &str>, recognize!(tuple!(opt!(tuple!(userinfo, tag!("@"))), host, opt!(tuple!(tag!(":"), port)))));
//* userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
named!(userinfo<&str, &str>, recognize!(many0!(alt_complete!(unreserved | pct_encoded | sub_delims | tag!(":")))));
//* host          = IP-literal / IPv4address / reg-name
named!(host<&str, &str>, alt_complete!(IP_literal | IPv4address | reg_name));
//* port          = *DIGIT
named!(port<&str, &str>, recognize!(many0!(DIGIT)));
//* IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
named!(IP_literal<&str, &str>, delimited!(tag!("["), alt_complete!(IPv6address | IPvFuture), tag!("]")));
//* IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
named!(IPvFuture<&str, &str>, recognize!(tuple!(tag!("v"), many1!(HEXDIG), tag!("."), many1!(alt_complete!(unreserved | sub_delims | tag!(":"))))));
//* IPv6address   =                            6( h16 ":" ) ls32
//*                  /                       "::" 5( h16 ":" ) ls32
//*                  / [               h16 ] "::" 4( h16 ":" ) ls32
//*                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
//*                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
//*                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
//*                  / [ *4( h16 ":" ) h16 ] "::"              ls32
//*                  / [ *5( h16 ":" ) h16 ] "::"              h16
//*                  / [ *6( h16 ":" ) h16 ] "::"
named!(IPv6address<&str, &str>, alt_complete!(
		recognize!(tuple!(                                                                        many_m_n!(6, 6, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(                                                            tag!("::"), many_m_n!(5, 5, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(                                                h16 ), tag!("::"), many_m_n!(4, 4, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 1, tuple!(h16, tag!(":"))), h16)), tag!("::"), many_m_n!(3, 3, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 2, tuple!(h16, tag!(":"))), h16)), tag!("::"), many_m_n!(2, 2, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 3, tuple!(h16, tag!(":"))), h16)), tag!("::"),                        h16, tag!(":"),   ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 4, tuple!(h16, tag!(":"))), h16)), tag!("::"),                                          ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 5, tuple!(h16, tag!(":"))), h16)), tag!("::"),                                           h16)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 6, tuple!(h16, tag!(":"))), h16)), tag!("::")                                               ))
));
//* h16           = 1*4HEXDIG
named!(h16<&str, &str>, recognize!(many1!(many_m_n!(4, 4, HEXDIG))));
//* ls32          = ( h16 ":" h16 ) / IPv4address
named!(ls32<&str, &str>, alt_complete!(recognize!(separated_pair!(h16, tag!(":"), h16)) | IPv4address));
//* IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
named!(IPv4address<&str, &str>, recognize!(tuple!(dec_octet, tag!("."), dec_octet, tag!("."), dec_octet, tag!("."), dec_octet)));
//* dec-octet     = "1" 2DIGIT            ; 100-199
//*               / "2" %x30-34 DIGIT     ; 200-249
//*               / "25" %x30-35          ; 250-255
//*               / %x31-39 DIGIT         ; 10-99
//*               / DIGIT                 ; 0-9
named!(dec_octet<&str, &str>, alt_complete!(
	recognize!(tuple!(tag!("1"), DIGIT, DIGIT))            |
	recognize!(tuple!(tag!("2"), one_of!("01234"), DIGIT)) |
	recognize!(tuple!(tag!("25"), one_of!("012345")))      |
	recognize!(tuple!(one_of!("123456789"), DIGIT))        |
	DIGIT
));
//* reg-name      = *( unreserved / pct-encoded / sub-delims )
named!(reg_name<&str, &str>, recognize!(many0!(alt_complete!(unreserved | pct_encoded | sub_delims))));
//* ;path          = path-abempty    ; begins with "/" or is empty
//* ;              / path-absolute   ; begins with "/" but not "//"
//* ;              / path-noscheme   ; begins with a non-colon segment
//* ;              / path-rootless   ; begins with a segment
//* ;              / path-empty      ; zero characters
//* path-abempty  = *( "/" segment )
named!(path_abempty<&str, &str>, recognize!(many0!(preceded!(tag!("/"), segment))));
//* path-absolute = "/" [ segment-nz *( "/" segment ) ]
named!(path_absolute<&str, &str>, recognize!(tuple!(tag!("/"), opt!(tuple!(segment_nz, path_abempty)))));
//* ;path-noscheme = segment-nz-nc *( "/" segment )
//* path-rootless = segment-nz *( "/" segment )
named!(path_rootless<&str, &str>, recognize!(tuple!(segment_nz, path_abempty)));
//* ;path-empty    = ""
named!(path_empty<&str, &str>, tag!(""));
//* segment       = *pchar
named!(segment<&str, &str>, recognize!(many0!(pchar)));
//* segment-nz    = 1*pchar
named!(segment_nz<&str, &str>, recognize!(many1!(pchar)));
//* ;segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
//* pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
named!(pchar<&str, &str>, alt_complete!(unreserved | pct_encoded | sub_delims | recognize!(one_of!(":@"))));
//* query         = *( pchar / "/" / "?" )
named!(query<&str, &str>, recognize!(many0!(alt_complete!(pchar | recognize!(one_of!("/?"))))));
//* fragment      = *( pchar / "/" / "?" )
named!(fragment<&str, &str>, recognize!(many0!(alt_complete!(pchar | recognize!(one_of!("/?"))))));
//* pct-encoded   = "%" HEXDIG HEXDIG
named!(pct_encoded<&str, &str>, recognize!(do_parse!(tag!("%") >> HEXDIG >> HEXDIG >> ())));
//* unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
named!(unreserved<&str, &str>, alt_complete!(ALPHA | DIGIT | recognize!(one_of!("-._~"))));
//* ;reserved      = gen-delims / sub-delims
//* ;gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
//* ;sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
//* sub-delims     =       "$" / "&" / "'" /                                     "=" / other-delims
named!(sub_delims<&str, &str>, alt_complete!(recognize!(one_of!("$&'=")) | other_delims));
//* other-delims   = "!" /                   "(" / ")" / "*" / "+" / "," / ";"
named!(other_delims<&str, &str>, recognize!(one_of!("!()*+,;")));
//*
//* pchar-no-SQUOTE       = unreserved / pct-encoded-no-SQUOTE / other-delims / "$" / "&" / "=" / ":" / "@"
named!(pchar_no_SQUOTE<&str, &str>, alt_complete!(unreserved | pct_encoded_no_SQUOTE | other_delims | recognize!(one_of!("$&=:@"))));
//* pct-encoded-no-SQUOTE = "%" ( "0" / "1" /   "3" / "4" / "5" / "6" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" / "2" / "3" / "4" / "5" / "6" /   "8" / "9" / A-to-F )
named!(pct_encoded_no_SQUOTE<&str, &str>, alt_complete!(  recognize!(tuple!(tag!("%"), one_of!("013456789ABCDEFabcdef"), HEXDIG))
					       | recognize!(tuple!(tag!("%2"), one_of!("012345689ABCDEFabcdef")))
					      ));
//*
//* qchar-no-AMP              = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_no_AMP<&str, &str>, alt_complete!(qchar_no_AMP_EQ_AT_DOLLAR | tag!("=")));
//* qchar-no-AMP-EQ           = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'"
named!(qchar_no_AMP_EQ<&str, &str>, alt_complete!(qchar_no_AMP_EQ_AT_DOLLAR | tag!("@") | tag!("$")));
//* qchar-no-AMP-EQ-AT-DOLLAR = unreserved / pct-encoded / other-delims / ":" /       "/" / "?" /       "'"
named!(qchar_no_AMP_EQ_AT_DOLLAR<&str, &str>, alt_complete!(unreserved | pct_encoded | other_delims | recognize!(one_of!(":/?'"))));
//*
//* qchar-unescaped       = unreserved / pct-encoded-unescaped / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_unescaped<&str, &str>, alt_complete!(unreserved | pct_encoded_unscaped | other_delims | recognize!(one_of!(":@/?$'="))));
//* pct-encoded-unescaped = "%" ( "0" / "1" /   "3" / "4" /   "6" / "7" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" /   "3" / "4" / "5" / "6" / "7" / "8" / "9" / A-to-F )
//*                       / "%" "5" ( DIGIT / "A" / "B" /   "D" / "E" / "F" )
named!(pct_encoded_unscaped<&str, &str>, alt_complete!( recognize!(tuple!(tag!("%"), alt_complete!(recognize!(one_of!("01346789")) | A_to_F), HEXDIG))
					     | recognize!(tuple!(tag!("%2"), alt_complete!(recognize!(one_of!("013456789")) | A_to_F)))
					     | recognize!(tuple!(tag!("%5"), alt_complete!(DIGIT | recognize!(one_of!("ABDEFabdef")))))
					     ));

//*
//* qchar-no-AMP-DQUOTE   = qchar-unescaped
//*                       / escape ( escape / quotation-mark )
named!(qchar_no_AMP_DQUOTE<&str, &str>, alt_complete!(qchar_unescaped | recognize!(tuple!(escape, alt_complete!(escape | quotation_mark)))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; B. IRI syntax [RFC3987]
//* ;------------------------------------------------------------------------------
//* ; Note: these are over-generous stubs, for the actual patterns refer to RFC3987
//* ;------------------------------------------------------------------------------
//*
//* IRI-in-header = 1*( VCHAR / obs-text )
named!(IRI_in_header<&str, &str>, recognize!(many1!(alt_complete!(VCHAR | obs_text))));
//* IRI-in-query  = 1*qchar-no-AMP
named!(IRI_in_query<&str, &str>, recognize!(many1!(qchar_no_AMP)));

//* ;------------------------------------------------------------------------------
//* ; C. ABNF core definitions [RFC5234]
//* ;------------------------------------------------------------------------------
//*
//* ALPHA  = %x41-5A / %x61-7A
named!(ALPHA<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_alphabetic(chr as u8))));

//* DIGIT  = %x30-39
named!(DIGIT<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_digit(chr as u8))));
//
// //* HEXDIG = DIGIT / A-to-F
named!(HEXDIG<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_hex_digit(chr as u8))));

//* A-to-F = "A" / "B" / "C" / "D" / "E" / "F"
fn is_A_to_F(chr: u8) -> bool {
	(chr >= 0x41 && chr <= 0x46) || (chr >= 0x61 && chr <= 0x66)
}
named!(A_to_F<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && is_A_to_F(chr as u8))));

//* DQUOTE = %x22
named!(DQUOTE<&str, &str>, tag!("\u{0022}"));

//* SP     = %x20
named!(SP<&str, &str>, tag!("\u{0020}"));

//* HTAB   = %x09
named!(HTAB<&str, &str>, tag!("\u{0009}"));

//* ;WSP    = SP / HTAB
//* ;LWSP = *(WSP / CRLF WSP)
//* VCHAR = %x21-7E
named!(VCHAR<&str, &str>, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii_graphic())));

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
