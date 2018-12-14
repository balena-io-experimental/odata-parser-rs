#![allow(non_snake_case)]
#![recursion_limit="128"]

#[macro_use] extern crate nom;
#[macro_use] extern crate nom_trace;

declare_trace!();

use std::str;

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
named!(odataUri<&str, &str>, tr!(odataUri, recognize!(tuple!(serviceRoot, opt!(odataRelativeUri)))));
//*
//* serviceRoot = ( "https" / "http" )                    ; Note: case-insensitive
//*               "://" host [ ":" port ]
//*               "/" *( segment-nz "/" )
named!(serviceRoot<&str, &str>, tr!(serviceRoot, recognize!(tuple!(alt!(tag_no_case!("https") | tag_no_case!("http")),
						  tag!("://"), host, opt!(tuple!(tag!(":"), port)),
						  tag!("/"), many0!(tuple!(segment_nz, tag!("/")))
						  ))));
//*
//* ; Note: dollar-prefixed path segments are case-sensitive!
//* odataRelativeUri = '$batch'  [ "?" batchOptions ]
//*                  / '$entity' "?" entityOptions
//*                  / '$entity' "/" qualifiedEntityTypeName "?" entityCastOptions
//*                  / '$metadata' [ "?" metadataOptions ] [ context ]
//*                  / resourcePath [ "?" queryOptions ]
named!(odataRelativeUri<&str, &str>, tr!(odataRelativeUri, alt!(recognize!(tuple!(tag!("$batch"), opt!(tuple!(tag!("?"), batchOptions))))
					  | recognize!(tuple!(tag!("$entity"), tag!("?"), batchOptions))
					  | recognize!(tuple!(tag!("$entity/"), qualifiedEntityTypeName, tag!("?"), entityCastOptions))
					  | recognize!(tuple!(tag!("$metadata"), opt!(tuple!(tag!("?"), metadataOptions)), opt!(context)))
					  | recognize!(tuple!(resourcePath, opt!(tuple!(tag!("?"), queryOptions))))
					  )));
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
named!(resourcePath<&str, &str>, tr!(resourcePath, alt!(recognize!(tuple!(entitySetName, opt!(collectionNavigation)))
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
				      )));
//*
//* collectionNavigation = [ "/" qualifiedEntityTypeName ] [ collectionNavPath ]
named!(collectionNavigation<&str, &str>, tr!(collectionNavigation, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(collectionNavPath)))));
//* collectionNavPath    = keyPredicate [ singleNavigation ]
//*                      / filterInPath [ collectionNavigation ]
//*                      / each [ boundOperation ]
//*                      / boundOperation
//*                      / count
//*                      / ref
named!(collectionNavPath<&str, &str>, tr!(collectionNavPath, alt!(recognize!(tuple!(keyPredicate, opt!(singleNavigation)))
					   | recognize!(tuple!(filterInPath, opt!(collectionNavigation)))
					   | recognize!(tuple!(each, opt!(boundOperation)))
					   | boundOperation
					   | count
					   | _ref
					)));
//*
//* keyPredicate     = simpleKey / compoundKey / keyPathSegments
named!(keyPredicate<&str, &str>, tr!(keyPredicate, alt!(simpleKey | compoundKey | keyPathSegments)));
//* simpleKey        = OPEN ( parameterAlias / keyPropertyValue ) CLOSE
named!(simpleKey<&str, &str>, tr!(simpleKey, recognize!(tuple!(OPEN, alt!(parameterAlias | keyPropertyValue), CLOSE))));
//* compoundKey      = OPEN keyValuePair *( COMMA keyValuePair ) CLOSE
named!(compoundKey<&str, &str>, tr!(compoundKey, recognize!(tuple!(OPEN, keyValuePair, many0!(tuple!(COMMA, keyValuePair)), CLOSE))));
//* keyValuePair     = ( primitiveKeyProperty / keyPropertyAlias  ) EQ ( parameterAlias / keyPropertyValue )
named!(keyValuePair<&str, &str>, tr!(keyValuePair, recognize!(tuple!(alt!(primitiveKeyProperty | keyPropertyAlias), EQ, alt!(parameterAlias | keyPropertyValue)))));
//* keyPropertyValue = primitiveLiteral
named!(keyPropertyValue<&str, &str>, tr!(keyPropertyValue, recognize!(primitiveLiteral)));
//* keyPropertyAlias = odataIdentifier
named!(keyPropertyAlias<&str, &str>, tr!(keyPropertyAlias, recognize!(odataIdentifier)));
//* keyPathSegments  = 1*( "/" keyPathLiteral )
named!(keyPathSegments<&str, &str>, tr!(keyPathSegments, recognize!(many1!(tuple!(tag!("/"), keyPathLiteral)))));
//* keyPathLiteral   = *pchar
named!(keyPathLiteral<&str, &str>, tr!(keyPathLiteral, recognize!(many0!(pchar))));
//*
//* singleNavigation = [ "/" qualifiedEntityTypeName ]
//*                    [ "/" propertyPath
//*                    / boundOperation
//*                    / ref
//*                    / value  ; request the media resource of a media entity
//*                    ]
named!(singleNavigation<&str, &str>, tr!(singleNavigation, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(alt!(recognize!(tuple!(tag!("/"), propertyPath))
														   | boundOperation
														   | _ref
														   | value
														   ))))));
//*
//* propertyPath = entityColNavigationProperty [ collectionNavigation ]
//*              / entityNavigationProperty    [ singleNavigation ]
//*              / complexColProperty          [ complexColPath ]
//*              / complexProperty             [ complexPath ]
//*              / primitiveColProperty        [ primitiveColPath ]
//*              / primitiveProperty           [ primitivePath ]
//*              / streamProperty              [ boundOperation ]
named!(propertyPath<&str, &str>, tr!(propertyPath, recognize!(alt!(tuple!(entityColNavigationProperty, opt!(collectionNavigation))
						 | tuple!(entityNavigationProperty, opt!(singleNavigation))
						 | tuple!(complexColProperty, opt!(complexColPath))
						 | tuple!(complexProperty, opt!(complexPath))
						 | tuple!(primitiveColProperty, opt!(primitiveColPath))
						 | tuple!(primitiveProperty, opt!(primitivePath))
						 | tuple!(streamProperty, opt!(boundOperation))
						 ))));

//*
//* primitiveColPath = count / boundOperation / ordinalIndex
named!(primitiveColPath<&str, &str>, tr!(primitiveColPath, alt!(count | boundOperation | ordinalIndex)));
//*
//* primitivePath  = value / boundOperation
named!(primitivePath<&str, &str>, tr!(primitivePath, alt!(value | boundOperation)));
//*
//* complexColPath = ordinalIndex
//*                / [ "/" qualifiedComplexTypeName ] [ count / boundOperation ]
named!(complexColPath<&str, &str>, tr!(complexColPath, alt!(ordinalIndex | recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(alt!(count | boundOperation)))))));
//*
//* complexPath    = [ "/" qualifiedComplexTypeName ]
//*                  [ "/" propertyPath
//*                  / boundOperation
//*                  ]
named!(complexPath<&str, &str>, tr!(complexPath, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(alt!(recognize!(tuple!(tag!("/"), propertyPath)) | boundOperation))))));
//*
//* filterInPath = '/$filter' EQ parameterAlias
named!(filterInPath<&str, &str>, tr!(filterInPath, recognize!(tuple!(tag!("/$filter"), EQ, parameterAlias))));
//*
//* each  = '/$each'
named!(each<&str, &str>, tr!(each, tag!("/$each")));
//* count = '/$count'
named!(count<&str, &str>, tr!(count, tag!("/$count")));
//* ref   = '/$ref'
named!(_ref<&str, &str>, tr!(_ref, tag!("/$ref")));
//* value = '/$value'
named!(value<&str, &str>, tr!(value, tag!("/$value")));
//*
//* ordinalIndex = "/" 1*DIGIT
named!(ordinalIndex<&str, &str>, tr!(ordinalIndex, recognize!(tuple!(tag!("/"), many1!(DIGIT)))));
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
named!(boundOperation<&str, &str>, tr!(boundOperation, recognize!(tuple!(tag!("/"), alt!(boundActionCall
								     | recognize!(tuple!(boundEntityColFunctionCall, opt!(collectionNavigation)))
								     | recognize!(tuple!(boundEntityFunctionCall, opt!(singleNavigation)))
								     | recognize!(tuple!(boundComplexColFunctionCall, opt!(complexColPath)))
								     | recognize!(tuple!(boundComplexFunctionCall, opt!(complexPath)))
								     | recognize!(tuple!(boundPrimitiveColFunctionCall, opt!(primitiveColPath)))
								     | recognize!(tuple!(boundPrimitiveFunctionCall, opt!(primitivePath)))
								     | boundFunctionCallNoParens
								     )))));
//*
//* actionImportCall = actionImport
named!(actionImportCall<&str, &str>, tr!(actionImportCall, recognize!(actionImport)));
//* boundActionCall  = namespace "." action
//*                    ; with the added restriction that the binding parameter MUST be either an entity or collection of entities
//*                    ; and is specified by reference using the URI immediately preceding (to the left) of the boundActionCall
named!(boundActionCall<&str, &str>, tr!(boundActionCall, recognize!(tuple!(namespace, tag!("."), action))));
//*
//* ; The following boundXxxFunctionCall rules have the added restrictions that
//* ;  - the function MUST support binding, and
//* ;  - the binding parameter type MUST match the type of resource identified by the
//* ;    URI immediately preceding (to the left) of the boundXxxFunctionCall, and
//* ;  - the functionParameters MUST NOT include the bindingParameter.
//TODO(validate)
//* boundEntityFunctionCall       = namespace "." entityFunction       functionParameters
named!(boundEntityFunctionCall<&str, &str>, tr!(boundEntityFunctionCall, recognize!(tuple!(entityFunction, functionParameters))));
//* boundEntityColFunctionCall    = namespace "." entityColFunction    functionParameters
named!(boundEntityColFunctionCall<&str, &str>, tr!(boundEntityColFunctionCall, recognize!(tuple!(entityColFunction, functionParameters))));
//* boundComplexFunctionCall      = namespace "." complexFunction      functionParameters
named!(boundComplexFunctionCall<&str, &str>, tr!(boundComplexFunctionCall, recognize!(tuple!(complexFunction, functionParameters))));
//* boundComplexColFunctionCall   = namespace "." complexColFunction   functionParameters
named!(boundComplexColFunctionCall<&str, &str>, tr!(boundComplexColFunctionCall, recognize!(tuple!(complexColFunction, functionParameters))));
//* boundPrimitiveFunctionCall    = namespace "." primitiveFunction    functionParameters
named!(boundPrimitiveFunctionCall<&str, &str>, tr!(boundPrimitiveFunctionCall, recognize!(tuple!(primitiveFunction, functionParameters))));
//* boundPrimitiveColFunctionCall = namespace "." primitiveColFunction functionParameters
named!(boundPrimitiveColFunctionCall<&str, &str>, tr!(boundPrimitiveColFunctionCall, recognize!(tuple!(primitiveColFunction, functionParameters))));
//*
//* boundFunctionCallNoParens     = namespace "." entityFunction
//*                               / namespace "." entityColFunction
//*                               / namespace "." complexFunction
//*                               / namespace "." complexColFunction
//*                               / namespace "." primitiveFunction
//*                               / namespace "." primitiveColFunction
named!(boundFunctionCallNoParens<&str, &str>, tr!(boundFunctionCallNoParens, recognize!(alt!(tuple!(namespace, tag!("."), entityFunction)
							      | tuple!(namespace, tag!("."), entityColFunction)
						   	      | tuple!(namespace, tag!("."), complexFunction)
						   	      | tuple!(namespace, tag!("."), complexColFunction)
						   	      | tuple!(namespace, tag!("."), primitiveFunction)
						   	      | tuple!(namespace, tag!("."), primitiveColFunction)
							 ))));

//*
//* entityFunctionImportCall       = entityFunctionImport       functionParameters
named!(entityFunctionImportCall<&str, &str>, tr!(entityFunctionImportCall, recognize!(tuple!(entityFunctionImport,functionParameters))));
//* entityColFunctionImportCall    = entityColFunctionImport    functionParameters
named!(entityColFunctionImportCall<&str, &str>, tr!(entityColFunctionImportCall, recognize!(tuple!(entityColFunctionImport,functionParameters))));
//* complexFunctionImportCall      = complexFunctionImport      functionParameters
named!(complexFunctionImportCall<&str, &str>, tr!(complexFunctionImportCall, recognize!(tuple!(complexFunctionImport,functionParameters))));
//* complexColFunctionImportCall   = complexColFunctionImport   functionParameters
named!(complexColFunctionImportCall<&str, &str>, tr!(complexColFunctionImportCall, recognize!(tuple!(complexColFunctionImport,functionParameters))));
//* primitiveFunctionImportCall    = primitiveFunctionImport    functionParameters
named!(primitiveFunctionImportCall<&str, &str>, tr!(primitiveFunctionImportCall, recognize!(tuple!(primitiveFunctionImport,functionParameters))));
//* primitiveColFunctionImportCall = primitiveColFunctionImport functionParameters
named!(primitiveColFunctionImportCall<&str, &str>, tr!(primitiveColFunctionImportCall, recognize!(tuple!(primitiveColFunctionImport,functionParameters))));
//*
//* functionImportCallNoParens     = entityFunctionImport
//*                                / entityColFunctionImport
//*                                / complexFunctionImport
//*                                / complexColFunctionImport
//*                                / primitiveFunctionImport
//*                                / primitiveColFunctionImport
named!(functionImportCallNoParens<&str, &str>, tr!(functionImportCallNoParens, alt!(entityFunctionImport
						    | entityColFunctionImport
						    | complexFunctionImport
						    | complexColFunctionImport
						    | primitiveFunctionImport
						    | primitiveColFunctionImport)));
//*
//* functionParameters = OPEN [ functionParameter *( COMMA functionParameter ) ] CLOSE
named!(functionParameters<&str, &str>, tr!(functionParameters, recognize!(tuple!(OPEN, opt!(tuple!(functionParameter, many0!(tuple!(COMMA, functionParameter)))), CLOSE))));
//* functionParameter  = parameterName EQ ( parameterAlias / primitiveLiteral )
named!(functionParameter<&str, &str>, tr!(functionParameter, recognize!(tuple!(parameterName, EQ, alt!(parameterAlias | primitiveLiteral)))));
//* parameterName      = odataIdentifier
named!(parameterName<&str, &str>, tr!(parameterName, recognize!(odataIdentifier)));
//* parameterAlias     = AT odataIdentifier
named!(parameterAlias<&str, &str>, tr!(parameterAlias, recognize!(tuple!(AT, odataIdentifier))));
//*
//* crossjoin = '$crossjoin' OPEN
//*             entitySetName *( COMMA entitySetName )
//*             CLOSE
named!(crossjoin<&str, &str>, tr!(crossjoin, recognize!(tuple!(tag!("$crossjoin"), OPEN, entitySetName, many0!(tuple!(COMMA, entitySetName)), CLOSE))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 2. Query Options
//* ;------------------------------------------------------------------------------
//*
//* queryOptions = queryOption *( "&" queryOption )
named!(queryOptions<&str, &str>, tr!(queryOptions, recognize!(tuple!(queryOption, many0!(tuple!(tag!("&"), queryOption))))));
//* queryOption  = systemQueryOption
//*              / aliasAndValue
//*              / nameAndValue
//*              / customQueryOption
named!(queryOption<&str, &str>, tr!(queryOption, alt!(systemQueryOption | aliasAndValue | nameAndValue | customQueryOption)));
//*
//* batchOptions = batchOption *( "&" batchOption )
named!(batchOptions<&str, &str>, tr!(batchOptions, recognize!(tuple!(batchOption, many0!(tuple!(tag!("&"), batchOption))))));
//* batchOption  = format
//*              /customQueryOption
named!(batchOption<&str, &str>, tr!(batchOption, alt!(format | customQueryOption)));
//*
//* metadataOptions = metadataOption *( "&" metadataOption )
named!(metadataOptions<&str, &str>, tr!(metadataOptions, recognize!(tuple!(metadataOption, many0!(tuple!(tag!("&"), metadataOption))))));
//* metadataOption  = format
//*                 /customQueryOption
named!(metadataOption<&str, &str>, tr!(metadataOption, alt!(format | customQueryOption)));
//*
//* entityOptions  = *( entityIdOption "&" ) id *( "&" entityIdOption )
named!(entityOptions<&str, &str>, tr!(entityOptions, recognize!(tuple!(many0!(tuple!(entityIdOption, tag!("&"))), id, many0!(tuple!(tag!("&"), entityIdOption))))));
//* entityIdOption = format
//*                / customQueryOption
named!(entityIdOption<&str, &str>, tr!(entityIdOption, alt!(format | customQueryOption)));
//* entityCastOptions = *( entityCastOption "&" ) id *( "&" entityCastOption )
named!(entityCastOptions<&str, &str>, tr!(entityCastOptions, recognize!(tuple!(many0!(tuple!(entityCastOption, tag!("&"))), id, many0!(tuple!(tag!("&"), entityCastOption))))));
//* entityCastOption  = entityIdOption
//*                   / expand
//*                   / select
named!(entityCastOption<&str, &str>, tr!(entityCastOption, alt!(entityIdOption | expand | select)));
//*
//* id = ( "$id" / "id" ) EQ IRI-in-query
named!(id<&str, &str>, tr!(id, recognize!(tuple!(alt!(tag_no_case!("$id") | tag_no_case!("id")), EQ, IRI_in_query))));
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
named!(systemQueryOption<&str, &str>, tr!(systemQueryOption, alt!(compute
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
					   | index)));
//*
//* compute          = ( "$compute" / "compute" ) EQ computeItem *( COMMA computeItem )
named!(compute<&str, &str>, tr!(compute, recognize!(tuple!(alt!(tag_no_case!("$compute") | tag_no_case!("compute")), EQ, computeItem, many0!(tuple!(COMMA, computeItem))))));
//* computeItem      = commonExpr RWS "as" RWS computedProperty
named!(computeItem<&str, &str>, tr!(computeItem, recognize!(tuple!(commonExpr, RWS, tag_no_case!("as"), RWS, computedProperty))));
//* computedProperty = odataIdentifier
named!(computedProperty<&str, &str>, tr!(computedProperty, recognize!(odataIdentifier)));
//*
//* expand            = ( "$expand" / "expand" ) EQ expandItem *( COMMA expandItem )
named!(expand<&str, &str>, tr!(expand, recognize!(tuple!(alt!(tag_no_case!("$expand") | tag_no_case!("expand")), EQ, expandItem, many0!(tuple!(COMMA, expandItem))))));
//* expandItem        = STAR [ ref / OPEN levels CLOSE ]
//*                   / "$value"
//*                   / expandPath
//*                     [ ref   [ OPEN expandRefOption   *( SEMI expandRefOption   ) CLOSE ]
//*                     / count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                     /         OPEN expandOption      *( SEMI expandOption      ) CLOSE
//*                     ]
named!(expandItem<&str, &str>, tr!(expandItem, recognize!(alt!(recognize!(tuple!(STAR, opt!(alt!(_ref | recognize!(tuple!(OPEN, levels, CLOSE))))))
					       | tag_no_case!("$value")
					       | recognize!(tuple!(expandPath,
								   opt!(alt!(
									     recognize!(tuple!(_ref, opt!(tuple!(OPEN, expandRefOption, many0!(tuple!(SEMI, expandRefOption)), CLOSE))))
									     | recognize!(tuple!(count, opt!(tuple!(OPEN, expandCountOption, many0!(tuple!(SEMI, expandCountOption)), CLOSE))))
									     | recognize!(tuple!(OPEN, expandOption, many0!(tuple!(SEMI, expandOption)), CLOSE))
									 ))
								  ))
						))));


//* expandPath        = [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                     *( ( complexProperty / complexColProperty ) "/" [ qualifiedComplexTypeName "/" ] )
//*                     ( STAR / streamProperty / navigationProperty [ "/" qualifiedEntityTypeName ] )
named!(expandPath<&str, &str>, tr!(expandPath, recognize!(tuple!(
						opt!(tuple!(alt!(qualifiedEntityTypeName | qualifiedComplexTypeName), tag!("/"))),
						many0!(tuple!(alt!(complexProperty | complexColProperty), tag!("/"), opt!(tuple!(qualifiedComplexTypeName, tag!("/"))))),
						alt!(STAR | streamProperty | recognize!(tuple!(navigationProperty, opt!(tuple!(tag!("/"), qualifiedEntityTypeName)))))
						))));

//* expandCountOption = filter
//*                   / search
named!(expandCountOption<&str, &str>, tr!(expandCountOption, alt!(filter | search)));
//* expandRefOption   = expandCountOption
//*                   / orderby
//*                   / skip
//*                   / top
//*                   / inlinecount
named!(expandRefOption<&str, &str>, tr!(expandRefOption, alt!(expandCountOption | orderby | skip | top | inlinecount)));
//* expandOption      = expandRefOption
//*                   / select
//*                   / expand
//*                   / compute
//*                   / levels
//*                   / aliasAndValue
named!(expandOption<&str, &str>, tr!(expandOption, alt!(expandRefOption | select | expand | compute | levels | aliasAndValue)));
//*
//* levels = ( "$levels" / "levels" ) EQ ( oneToNine *DIGIT / "max" )
named!(levels<&str, &str>, tr!(levels, recognize!(tuple!(alt!(tag_no_case!("$levels") | tag_no_case!("levels")), EQ, alt!(recognize!(tuple!(oneToNine, many0!(DIGIT))) | tag_no_case!("max"))))));
//*
//* filter = ( "$filter" / "filter" ) EQ boolCommonExpr
named!(filter<&str, &str>, tr!(filter, recognize!(tuple!(alt!(tag_no_case!("$filter") | tag_no_case!("filter")), EQ, boolCommonExpr))));
//*
//* orderby     = ( "$orderby" / "orderby" ) EQ orderbyItem *( COMMA orderbyItem )
named!(orderby<&str, &str>, tr!(orderby, recognize!(tuple!(alt!(tag_no_case!("$orderby") | tag_no_case!("orderby")), EQ, orderbyItem, many0!(tuple!(COMMA, orderbyItem))))));
//* orderbyItem = commonExpr [ RWS ( "asc" / "desc" ) ]
named!(orderbyItem<&str, &str>, tr!(orderbyItem, recognize!(tuple!(commonExpr, opt!(tuple!(RWS, alt!(tag_no_case!("asc") | tag_no_case!("desc"))))))));
//*
//* skip = ( "$skip" / "skip" ) EQ 1*DIGIT
named!(skip<&str, &str>, tr!(skip, recognize!(tuple!(alt!(tag_no_case!("$skip") | tag_no_case!("skip")), EQ, many1!(DIGIT)))));
//* top  = ( "$top"  / "top"  ) EQ 1*DIGIT
named!(top<&str, &str>, tr!(top, recognize!(tuple!(alt!(tag_no_case!("$top") | tag_no_case!("top")), EQ, many1!(DIGIT)))));
//*
//* index  = ( "$index" / "index" ) EQ 1*DIGIT
named!(index<&str, &str>, tr!(index, recognize!(tuple!(alt!(tag_no_case!("$index") | tag_no_case!("index")), EQ, many1!(DIGIT)))));
//*
//* format = ( "$format" / "format" ) EQ
//*          ( "atom"
//*          / "json"
//*          / "xml"
//*          / 1*pchar "/" 1*pchar ; <a data service specific value indicating a
//*          )                     ; format specific to the specific data service> or
//*                                ; <An IANA-defined [IANA-MMT] content type>
named!(format<&str, &str>, tr!(format, recognize!(tuple!(
					     alt!(tag_no_case!("$format") | tag_no_case!("format")), EQ,
					     alt!(
						   tag_no_case!("atom")
						   | tag_no_case!("json")
						   | tag_no_case!("xml")
						   | tag_no_case!("xml")
						   | recognize!(tuple!(many1!(pchar), tag!("/"), many1!(pchar)))
					     )))));
//*
//* inlinecount = ( "$count" / "count" ) EQ booleanValue
named!(inlinecount<&str, &str>, tr!(inlinecount, recognize!(tuple!(alt!(tag_no_case!("$count") | tag_no_case!("count")), EQ, booleanValue))));
//*
//* schemaversion   = ( "$schemaversion" / "schemaversion" ) EQ ( STAR / 1*unreserved )
named!(schemaversion<&str, &str>, tr!(schemaversion, recognize!(tuple!(alt!(tag_no_case!("$schemaversion") | tag_no_case!("schemaversion")), EQ, alt!(STAR | recognize!(many1!(unreserved)))))));
//*
//* search     = ( "$search" / "search" ) EQ BWS searchExpr
named!(search<&str, &str>, tr!(search, recognize!(tuple!(alt!(tag_no_case!("$search") | tag_no_case!("search")), EQ, BWS, searchExpr))));
//* searchExpr = ( OPEN BWS searchExpr BWS CLOSE
//*              / searchTerm
//*              ) [ searchOrExpr
//*                / searchAndExpr
//*                ]
named!(searchExpr<&str, &str>, tr!(searchExpr, recognize!(tuple!(alt!(recognize!(tuple!(OPEN, BWS, searchExpr, BWS, CLOSE)) | searchTerm), opt!(alt!(searchOrExpr | searchAndExpr))))));
//*
//* searchOrExpr  = RWS 'OR'  RWS searchExpr
named!(searchOrExpr<&str, &str>, tr!(searchOrExpr, recognize!(tuple!(RWS, tag!("OR"), RWS, searchExpr))));
//* searchAndExpr = RWS [ 'AND' RWS ] searchExpr
named!(searchAndExpr<&str, &str>, tr!(searchAndExpr, recognize!(tuple!(RWS, opt!(tuple!(tag!("AND"), RWS)), searchExpr))));
//*
//* searchTerm   = [ 'NOT' RWS ] ( searchPhrase / searchWord )
named!(searchTerm<&str, &str>, tr!(searchTerm, recognize!(tuple!(opt!(tuple!(tag!("NOT"), RWS)), alt!(searchPhrase | searchWord)))));
//* searchPhrase = quotation-mark 1*qchar-no-AMP-DQUOTE quotation-mark
named!(searchPhrase<&str, &str>, tr!(searchPhrase, recognize!(tuple!(quotation_mark, many1!(qchar_no_AMP_DQUOTE), quotation_mark))));
//*
//* ; A searchWord is a sequence of one or more letters, digits, commas, or dots.
//* ; This includes Unicode characters of categories L or N using UTF-8 and percent-encoding.
//* ; The words AND, OR, and NOT are not a valid searchWord.
//* ; Expressing this in ABNF is somewhat clumsy, so the following rule is overly generous.
//TODO(validation)
//* searchWord   = 1*( ALPHA / DIGIT / COMMA / "." / pct-encoded )
named!(searchWord<&str, &str>, tr!(searchWord, recognize!(many1!(alt!(ALPHA | DIGIT | COMMA | tag!(".") | pct_encoded)))));
//*
//* select         = ( "$select" / "select" ) EQ selectItem *( COMMA selectItem )
named!(select<&str, &str>, tr!(select, recognize!(tuple!(alt!(tag_no_case!("$select") | tag_no_case!("select")), EQ, selectItem, many0!(tuple!(COMMA, selectItem))))));
//* selectItem     = STAR
//*                / allOperationsInSchema
//*                / [ ( qualifiedEntityTypeName / qualifiedComplexTypeName ) "/" ]
//*                  ( selectProperty
//*                  / qualifiedActionName
//*                  / qualifiedFunctionName
//*                  )
named!(selectItem<&str, &str>, tr!(selectItem, alt!(STAR
				    | allOperationsInSchema
				    | recognize!(tuple!(opt!(tuple!(alt!(qualifiedEntityTypeName | qualifiedComplexTypeName), tag!("/"))),
							alt!(selectProperty | qualifiedActionName | qualifiedFunctionName)
							))
				   )));
//* selectProperty = primitiveProperty
//*                / primitiveColProperty [ OPEN selectOptionPC *( SEMI selectOptionPC ) CLOSE ]
//*                / navigationProperty
//*                / selectPath [ OPEN selectOption *( SEMI selectOption ) CLOSE
//*                             / "/" selectProperty
//*                             ]
named!(selectProperty<&str, &str>, tr!(selectProperty, alt!(primitiveProperty
					| recognize!(tuple!(primitiveColProperty, opt!(tuple!(OPEN, selectOptionPC, many0!(tuple!(SEMI, selectOptionPC)), CLOSE))))
					| navigationProperty
					| recognize!(tuple!(selectPath, opt!(alt!(
										recognize!(tuple!(OPEN, selectOption, many0!(tuple!(SEMI, selectOption)), CLOSE))
										| recognize!(tuple!(tag!("/"), selectProperty))
										))))
					)));
//* selectPath     = ( complexProperty / complexColProperty ) [ "/" qualifiedComplexTypeName ]
named!(selectPath<&str, &str>, tr!(selectPath, recognize!(tuple!(alt!(complexProperty | complexColProperty), opt!(tuple!(tag!("/"), qualifiedComplexTypeName))))));
//* selectOptionPC = filter / search / inlinecount / orderby / skip / top
named!(selectOptionPC<&str, &str>, tr!(selectOptionPC, alt!(filter | search | inlinecount | orderby | skip | top)));
//* selectOption   = selectOptionPC
//*                / compute / select / expand / aliasAndValue
named!(selectOption<&str, &str>, tr!(selectOption, alt!(selectOptionPC | compute | select | expand | aliasAndValue)));
//*
//* allOperationsInSchema = namespace "." STAR
named!(allOperationsInSchema<&str, &str>, tr!(allOperationsInSchema, recognize!(tuple!(namespace, tag!("."), STAR))));
//*
//* ; The parameterNames uniquely identify the bound function overload
//* ; only if it has overloads.
//* qualifiedActionName   = namespace "." action
named!(qualifiedActionName<&str, &str>, tr!(qualifiedActionName, recognize!(tuple!(namespace, tag!("."), action))));
//* qualifiedFunctionName = namespace "." function [ OPEN parameterNames CLOSE ]
named!(qualifiedFunctionName<&str, &str>, tr!(qualifiedFunctionName, recognize!(tuple!(namespace, tag!("."), function, opt!(tuple!(OPEN, parameterNames, CLOSE))))));
//*
//* ; The names of all non-binding parameters, separated by commas
//* parameterNames = parameterName *( COMMA parameterName )
named!(parameterNames<&str, &str>, tr!(parameterNames, recognize!(tuple!(parameterName, many0!(tuple!(COMMA, parameterName))))));
//*
//* deltatoken = "$deltatoken" EQ 1*( qchar-no-AMP )
named!(deltatoken<&str, &str>, tr!(deltatoken, recognize!(tuple!(tag_no_case!("$deltatoken"), EQ, many1!(qchar_no_AMP)))));
//*
//* skiptoken = "$skiptoken" EQ 1*( qchar-no-AMP )
named!(skiptoken<&str, &str>, tr!(skiptoken, recognize!(tuple!(tag_no_case!("$skiptoken"), EQ, many1!(qchar_no_AMP)))));
//*
//* aliasAndValue = parameterAlias EQ parameterValue
named!(aliasAndValue<&str, &str>, tr!(aliasAndValue, recognize!(tuple!(parameterAlias, EQ, parameterValue))));
//*
//* nameAndValue = parameterName EQ parameterValue
named!(nameAndValue<&str, &str>, tr!(nameAndValue, recognize!(tuple!(parameterName, EQ, parameterValue))));
//*
//* parameterValue = arrayOrObject
//*                / commonExpr
named!(parameterValue<&str, &str>, tr!(parameterValue, alt!(arrayOrObject | commonExpr)));
//*
//* customQueryOption = customName [ EQ customValue ]
named!(customQueryOption<&str, &str>, tr!(customQueryOption, recognize!(tuple!(customName, opt!(tuple!(EQ, customValue))))));
//* customName        = qchar-no-AMP-EQ-AT-DOLLAR *( qchar-no-AMP-EQ )
named!(customName<&str, &str>, tr!(customName, recognize!(tuple!(qchar_no_AMP_EQ_AT_DOLLAR, many0!(qchar_no_AMP_EQ)))));
//* customValue       = *( qchar-no-AMP )
named!(customValue<&str, &str>, tr!(customValue, recognize!(many0!(qchar_no_AMP))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 3. Context URL Fragments
//* ;------------------------------------------------------------------------------
//*
//* context         = "#" contextFragment
named!(context<&str, &str>, tr!(context, recognize!(tuple!(tag!("#"), contextFragment))));
//* contextFragment = 'Collection($ref)'
//*                 / '$ref'
//*                 / 'Collection(Edm.EntityType)'
//*                 / 'Collection(Edm.ComplexType)'
//*                 / singletonEntity [ navigation *( containmentNavigation ) [ "/" qualifiedEntityTypeName ] ] [ selectList ]
//*                 / qualifiedTypeName [ selectList ]
//*                 / entitySet ( '/$deletedEntity' / '/$link' / '/$deletedLink' )
//*                 / entitySet keyPredicate "/" contextPropertyPath [ selectList ]
//*                 / entitySet [ selectList ] [ '/$entity' / '/$delta' ]
named!(contextFragment<&str, &str>, tr!(contextFragment, alt!(tag!("Collection($ref)")
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
					 | recognize!(tuple!(entitySet, alt!(tag!("/$deletedEntity") | tag!("/$link") | tag!("/$deletedLink"))))
					 | recognize!(tuple!(entitySet, keyPredicate, tag!("/"), contextPropertyPath, opt!(selectList)))
					 | recognize!(tuple!(entitySet, opt!(selectList), opt!(alt!(tag!("/$entity") | tag!("/$delta")))))
				    )));
//*
//* entitySet = entitySetName *( containmentNavigation ) [ "/" qualifiedEntityTypeName ]
named!(entitySet<&str, &str>, tr!(entitySet, recognize!(tuple!(entitySetName, many0!(containmentNavigation), opt!(tuple!(tag!("/"), qualifiedEntityTypeName))))));
//*
//* containmentNavigation = keyPredicate [ "/" qualifiedEntityTypeName ] navigation
named!(containmentNavigation<&str, &str>, tr!(containmentNavigation, recognize!(tuple!(keyPredicate, opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), navigation))));
//* navigation            = *( "/" complexProperty [ "/" qualifiedComplexTypeName ] ) "/" navigationProperty
named!(navigation<&str, &str>, tr!(navigation, recognize!(tuple!(many0!(tuple!(tag!("/"), complexProperty, opt!(tuple!(tag!("/"), qualifiedComplexTypeName)))), tag!("/"), navigationProperty))));
//*
//* selectList         = OPEN selectListItem *( COMMA selectListItem ) CLOSE
named!(selectList<&str, &str>, tr!(selectList, recognize!(tuple!(OPEN, selectListItem, many0!(tuple!(COMMA, selectListItem)), CLOSE))));
//* selectListItem     = STAR ; all structural properties
//*                    / allOperationsInSchema
//*                    / [ qualifiedEntityTypeName "/" ]
//*                      ( qualifiedActionName
//*                      / qualifiedFunctionName
//*                      / selectListProperty
//*                      )
named!(selectListItem<&str, &str>, tr!(selectListItem, alt!(STAR
					| allOperationsInSchema
					| recognize!(tuple!(opt!(tuple!(qualifiedEntityTypeName, tag!("/"))), alt!(qualifiedActionName | qualifiedFunctionName | selectListProperty)))
				       )));
//* selectListProperty = primitiveProperty
//*                    / primitiveColProperty
//*                    / navigationProperty [ "+" ] [ selectList ]
//*                    / selectPath [ "/" selectListProperty ]
named!(selectListProperty<&str, &str>, tr!(selectListProperty, alt!(primitiveProperty
					    | primitiveColProperty
					    | recognize!(tuple!(navigationProperty, opt!(tag!("+")), opt!(selectList)))
					    | recognize!(tuple!(selectPath, opt!(tuple!(tag!("/"), selectListProperty))))
					   )));
//*
//* contextPropertyPath = primitiveProperty
//*                     / primitiveColProperty
//*                     / complexColProperty
//*                     / complexProperty [ [ "/" qualifiedComplexTypeName ] "/" contextPropertyPath ]
named!(contextPropertyPath<&str, &str>, tr!(contextPropertyPath, alt!(primitiveProperty
					     | primitiveColProperty
					     | complexColProperty
					     | recognize!(tuple!(complexProperty, opt!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), tag!("/"), contextPropertyPath))))
					     )));
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
named!(commonExpr<&str, &str>, tr!(commonExpr, recognize!(tuple!(
						 alt!(
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
						 opt!(alt!(
							 addExpr
							 | subExpr
							 | mulExpr
							 | divExpr
							 | divbyExpr
							 | modExpr
						 )),
						 opt!(alt!(
							 eqExpr
							 | neExpr
							 | ltExpr
							 | leExpr
							 | gtExpr
							 | geExpr
							 | hasExpr
							 | inExpr
						 )),
						 opt!(alt!(andExpr | orExpr))
					))));
//*
//* boolCommonExpr = commonExpr ; resulting in a Boolean
//TODO(validate)
named!(boolCommonExpr<&str, &str>, tr!(boolCommonExpr, recognize!(commonExpr)));
//*
//* rootExpr = '$root/' ( entitySetName keyPredicate / singletonEntity ) [ singleNavigationExpr ]
named!(rootExpr<&str, &str>, tr!(rootExpr, recognize!(tuple!(tag!("$root/"), alt!(recognize!(tuple!(entitySetName, keyPredicate)) | singletonEntity), opt!(singleNavigationExpr)))));
//*
//* firstMemberExpr = memberExpr
//*                 / inscopeVariableExpr [ "/" memberExpr ]
named!(firstMemberExpr<&str, &str>, tr!(firstMemberExpr, alt!(memberExpr | recognize!(tuple!(inscopeVariableExpr, opt!(tuple!(tag!("/"), memberExpr)))))));
//*
//* memberExpr = [ qualifiedEntityTypeName "/" ]
//*              ( propertyPathExpr
//*              / boundFunctionExpr
//*              / annotationExpr
//*              )
named!(memberExpr<&str, &str>, tr!(memberExpr, recognize!(tuple!(opt!(tuple!(qualifiedEntityTypeName, tag!("/"))), alt!(propertyPathExpr | boundFunctionExpr | annotationExpr)))));
//*
//* propertyPathExpr = ( entityColNavigationProperty [ collectionNavigationExpr ]
//*                    / entityNavigationProperty    [ singleNavigationExpr ]
//*                    / complexColProperty          [ complexColPathExpr ]
//*                    / complexProperty             [ complexPathExpr ]
//*                    / primitiveColProperty        [ collectionPathExpr ]
//*                    / primitiveProperty           [ primitivePathExpr ]
//*                    / streamProperty              [ primitivePathExpr ]
//*                    )
named!(propertyPathExpr<&str, &str>, tr!(propertyPathExpr, recognize!(alt!(
						     tuple!(entityColNavigationProperty, opt!(collectionNavigationExpr))
						     | tuple!(entityNavigationProperty, opt!(singleNavigationExpr))
						     | tuple!(complexColProperty, opt!(complexColPathExpr))
						     | tuple!(complexProperty, opt!(complexPathExpr))
						     | tuple!(primitiveColProperty, opt!(collectionPathExpr))
						     | tuple!(primitiveProperty, opt!(primitivePathExpr))
						     | tuple!(streamProperty, opt!(primitivePathExpr))
						))));
//*
//* annotationExpr = annotation
//*                  [ collectionPathExpr
//*                  / singleNavigationExpr
//*                  / complexPathExpr
//*                  / primitivePathExpr
//*                  ]
named!(annotationExpr<&str, &str>, tr!(annotationExpr, recognize!(tuple!(annotation, opt!(alt!(collectionPathExpr | singleNavigationExpr | complexPathExpr | primitivePathExpr))))));
//*
//* annotation          = AT [ namespace "." ] termName [ '#' annotationQualifier ]
named!(annotation<&str, &str>, tr!(annotation, recognize!(tuple!(AT, opt!(tuple!(namespace, tag!("."))), termName, opt!(tuple!(tag!("#"), annotationQualifier))))));
//* annotationQualifier = odataIdentifier
named!(annotationQualifier<&str, &str>, tr!(annotationQualifier, recognize!(odataIdentifier)));
//*
//* inscopeVariableExpr  = implicitVariableExpr
//*                      / parameterAlias
//*                      / lambdaVariableExpr ; only allowed inside a lambdaPredicateExpr
//TODO(validation)
named!(inscopeVariableExpr<&str, &str>, tr!(inscopeVariableExpr, alt!(implicitVariableExpr | parameterAlias | lambdaVariableExpr)));
//* implicitVariableExpr = '$it'              ; the current instance of the resource identified by the resource path
//*                      / '$this'            ; the instance on which the query option is evaluated
named!(implicitVariableExpr<&str, &str>, tr!(implicitVariableExpr, alt!(tag!("$it") | tag!("$this"))));
//* lambdaVariableExpr   = odataIdentifier
named!(lambdaVariableExpr<&str, &str>, tr!(lambdaVariableExpr, recognize!(odataIdentifier)));
//*
//* collectionNavigationExpr = [ "/" qualifiedEntityTypeName ]
//*                            [ keyPredicate [ singleNavigationExpr ]
//*                            / collectionPathExpr
//*                            ]
named!(collectionNavigationExpr<&str, &str>, tr!(collectionNavigationExpr, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedEntityTypeName)), opt!(alt!(
															   recognize!(tuple!(keyPredicate, opt!(singleNavigationExpr)))
															   | collectionPathExpr
															))))));
//*
//* singleNavigationExpr = "/" memberExpr
named!(singleNavigationExpr<&str, &str>, tr!(singleNavigationExpr, recognize!(tuple!(tag!("/"), memberExpr))));
//*
//* complexColPathExpr = [ "/" qualifiedComplexTypeName ]
//*                      [ collectionPathExpr ]
named!(complexColPathExpr<&str, &str>, tr!(complexColPathExpr, recognize!(tuple!(opt!(tuple!(tag!("/"), qualifiedComplexTypeName)), opt!(collectionPathExpr)))));
//*
//* collectionPathExpr = count [ OPEN expandCountOption *( SEMI expandCountOption ) CLOSE ]
//*                    / "/" boundFunctionExpr
//*                    / "/" annotationExpr
//*                    / "/" anyExpr
//*                    / "/" allExpr
named!(collectionPathExpr<&str, &str>, tr!(collectionPathExpr, alt!(
					    recognize!(tuple!(count, opt!(tuple!(OPEN, expandCountOption, many0!(tuple!(SEMI, expandCountOption)), CLOSE))))
					    | recognize!(tuple!(tag!("/"), boundFunctionExpr))
					    | recognize!(tuple!(tag!("/"), annotationExpr))
					    | recognize!(tuple!(tag!("/"), anyExpr))
					    | recognize!(tuple!(tag!("/"), allExpr))
					   )));
//*
//* complexPathExpr = [ "/" qualifiedComplexTypeName ]
//*                   [ "/" propertyPathExpr
//*                   / "/" boundFunctionExpr
//*                   / "/" annotationExpr
//*                   ]
named!(complexPathExpr<&str, &str>, tr!(complexPathExpr, recognize!(tuple!(
						      opt!(tuple!(tag!("/"), qualifiedComplexTypeName)),
						      opt!(alt!(
								tuple!(tag!("/"), propertyPathExpr)
								| tuple!(tag!("/"), boundFunctionExpr)
								| tuple!(tag!("/"), annotationExpr)
						      ))
						))));
//*
//* primitivePathExpr = "/" [ annotationExpr / boundFunctionExpr ]
named!(primitivePathExpr<&str, &str>, tr!(primitivePathExpr, recognize!(tuple!(tag!("/"), opt!(alt!(annotationExpr | boundFunctionExpr))))));
//*
//* boundFunctionExpr = functionExpr ; boundFunction segments can only be composed if the type of the
//*                                  ; previous segment matches the type of the first function parameter
//TODO(validation)
named!(boundFunctionExpr<&str, &str>, tr!(boundFunctionExpr, recognize!(functionExpr)));
//*
//* functionExpr = namespace "."
//*                ( entityColFunction    functionExprParameters [ collectionNavigationExpr ]
//*                / entityFunction       functionExprParameters [ singleNavigationExpr ]
//*                / complexColFunction   functionExprParameters [ complexColPathExpr ]
//*                / complexFunction      functionExprParameters [ complexPathExpr ]
//*                / primitiveColFunction functionExprParameters [ collectionPathExpr ]
//*                / primitiveFunction    functionExprParameters [ primitivePathExpr ]
//*                )
named!(functionExpr<&str, &str>, tr!(functionExpr, recognize!(tuple!(namespace,
						   tag!("."),
						   alt!(
							recognize!(tuple!(entityColFunction, functionExprParameters, opt!(collectionNavigationExpr)))
							| recognize!(tuple!(entityFunction, functionExprParameters, opt!(singleNavigationExpr)))
							| recognize!(tuple!(complexColFunction, functionExprParameters, opt!(complexColPathExpr)))
							| recognize!(tuple!(complexFunction, functionExprParameters, opt!(complexPathExpr)))
							| recognize!(tuple!(primitiveColFunction, functionExprParameters, opt!(collectionPathExpr)))
							| recognize!(tuple!(primitiveFunction, functionExprParameters, opt!(primitivePathExpr)))
						   )))));
//*
//* functionExprParameters = OPEN [ functionExprParameter *( COMMA functionExprParameter ) ] CLOSE
named!(functionExprParameters<&str, &str>, tr!(functionExprParameters, recognize!(tuple!(OPEN, opt!(tuple!(functionExprParameter, many0!(tuple!(COMMA, functionExprParameter)))), CLOSE))));
//* functionExprParameter  = parameterName EQ ( parameterAlias / parameterValue )
named!(functionExprParameter<&str, &str>, tr!(functionExprParameter, recognize!(tuple!(parameterName, EQ, alt!(parameterAlias | parameterValue)))));
//*
//* anyExpr = "any" OPEN BWS [ lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr ] BWS CLOSE
named!(anyExpr<&str, &str>, tr!(anyExpr, recognize!(tuple!(tag_no_case!("any"), OPEN, BWS, opt!(tuple!(lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr)), BWS, CLOSE))));
//* allExpr = "all" OPEN BWS   lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr   BWS CLOSE
named!(allExpr<&str, &str>, tr!(allExpr, recognize!(tuple!(tag_no_case!("all"), OPEN, BWS, lambdaVariableExpr, BWS, COLON, BWS, lambdaPredicateExpr, BWS, CLOSE))));
//* lambdaPredicateExpr = boolCommonExpr ; containing at least one lambdaVariableExpr
//TODO(use verify!() to verify that it contains at least one lambdaVariableExpr)
named!(lambdaPredicateExpr<&str, &str>, tr!(lambdaPredicateExpr, recognize!(boolCommonExpr)));
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
named!(methodCallExpr<&str, &str>, tr!(methodCallExpr, alt!(indexOfMethodCallExpr
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
					| boolMethodCallExpr)));
//*
//* boolMethodCallExpr = endsWithMethodCallExpr
//*                    / startsWithMethodCallExpr
//*                    / containsMethodCallExpr
//*                    / intersectsMethodCallExpr
//*                    / hasSubsetMethodCallExpr
//*                    / hasSubsequenceMethodCallExpr
named!(boolMethodCallExpr<&str, &str>, tr!(boolMethodCallExpr, alt!(endsWithMethodCallExpr
					    | startsWithMethodCallExpr
					    | containsMethodCallExpr
					    | intersectsMethodCallExpr
					    | hasSubsetMethodCallExpr
					    | hasSubsequenceMethodCallExpr)));
//*
//* concatMethodCallExpr     = "concat"     OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(concatMethodCallExpr<&str, &str>, tr!(concatMethodCallExpr, recognize!(tuple!(tag_no_case!("concat"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* containsMethodCallExpr   = "contains"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(containsMethodCallExpr<&str, &str>, tr!(containsMethodCallExpr, recognize!(tuple!(tag_no_case!("contains"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* endsWithMethodCallExpr   = "endswith"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(endsWithMethodCallExpr<&str, &str>, tr!(endsWithMethodCallExpr, recognize!(tuple!(tag_no_case!("endswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* indexOfMethodCallExpr    = "indexof"    OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(indexOfMethodCallExpr<&str, &str>, tr!(indexOfMethodCallExpr, recognize!(tuple!(tag_no_case!("indexof"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* lengthMethodCallExpr     = "length"     OPEN BWS commonExpr BWS CLOSE
named!(lengthMethodCallExpr<&str, &str>, tr!(lengthMethodCallExpr, recognize!(tuple!(tag_no_case!("length"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* startsWithMethodCallExpr = "startswith" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(startsWithMethodCallExpr<&str, &str>, tr!(startsWithMethodCallExpr, recognize!(tuple!(tag_no_case!("startswith"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* substringMethodCallExpr  = "substring"  OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS [ COMMA BWS commonExpr BWS ] CLOSE
named!(substringMethodCallExpr<&str, &str>, tr!(substringMethodCallExpr, recognize!(tuple!(tag_no_case!("substring"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, opt!(tuple!(COMMA, BWS, commonExpr, BWS)), CLOSE))));
//* toLowerMethodCallExpr    = "tolower"    OPEN BWS commonExpr BWS CLOSE
named!(toLowerMethodCallExpr<&str, &str>, tr!(toLowerMethodCallExpr, recognize!(tuple!(tag_no_case!("tolower"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* toUpperMethodCallExpr    = "toupper"    OPEN BWS commonExpr BWS CLOSE
named!(toUpperMethodCallExpr<&str, &str>, tr!(toUpperMethodCallExpr, recognize!(tuple!(tag_no_case!("toupper"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* trimMethodCallExpr       = "trim"       OPEN BWS commonExpr BWS CLOSE
named!(trimMethodCallExpr<&str, &str>, tr!(trimMethodCallExpr, recognize!(tuple!(tag_no_case!("trim"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//*
//* yearMethodCallExpr               = "year"               OPEN BWS commonExpr BWS CLOSE
named!(yearMethodCallExpr<&str, &str>, tr!(yearMethodCallExpr, recognize!(tuple!(tag_no_case!("year"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* monthMethodCallExpr              = "month"              OPEN BWS commonExpr BWS CLOSE
named!(monthMethodCallExpr<&str, &str>, tr!(monthMethodCallExpr, recognize!(tuple!(tag_no_case!("month"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* dayMethodCallExpr                = "day"                OPEN BWS commonExpr BWS CLOSE
named!(dayMethodCallExpr<&str, &str>, tr!(dayMethodCallExpr, recognize!(tuple!(tag_no_case!("day"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* hourMethodCallExpr               = "hour"               OPEN BWS commonExpr BWS CLOSE
named!(hourMethodCallExpr<&str, &str>, tr!(hourMethodCallExpr, recognize!(tuple!(tag_no_case!("hour"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* minuteMethodCallExpr             = "minute"             OPEN BWS commonExpr BWS CLOSE
named!(minuteMethodCallExpr<&str, &str>, tr!(minuteMethodCallExpr, recognize!(tuple!(tag_no_case!("minute"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* secondMethodCallExpr             = "second"             OPEN BWS commonExpr BWS CLOSE
named!(secondMethodCallExpr<&str, &str>, tr!(secondMethodCallExpr, recognize!(tuple!(tag_no_case!("second"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* fractionalsecondsMethodCallExpr  = "fractionalseconds"  OPEN BWS commonExpr BWS CLOSE
named!(fractionalsecondsMethodCallExpr<&str, &str>, tr!(fractionalsecondsMethodCallExpr, recognize!(tuple!(tag_no_case!("fractionalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* totalsecondsMethodCallExpr       = "totalseconds"       OPEN BWS commonExpr BWS CLOSE
named!(totalsecondsMethodCallExpr<&str, &str>, tr!(totalsecondsMethodCallExpr, recognize!(tuple!(tag_no_case!("totalseconds"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* dateMethodCallExpr               = "date"               OPEN BWS commonExpr BWS CLOSE
named!(dateMethodCallExpr<&str, &str>, tr!(dateMethodCallExpr, recognize!(tuple!(tag_no_case!("date"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* timeMethodCallExpr               = "time"               OPEN BWS commonExpr BWS CLOSE
named!(timeMethodCallExpr<&str, &str>, tr!(timeMethodCallExpr, recognize!(tuple!(tag_no_case!("time"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* totalOffsetMinutesMethodCallExpr = "totaloffsetminutes" OPEN BWS commonExpr BWS CLOSE
named!(totalOffsetMinutesMethodCallExpr<&str, &str>, tr!(totalOffsetMinutesMethodCallExpr, recognize!(tuple!(tag_no_case!("totaloffsetminutes"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//*
//* minDateTimeMethodCallExpr = "mindatetime" OPEN BWS CLOSE
named!(minDateTimeMethodCallExpr<&str, &str>, tr!(minDateTimeMethodCallExpr, recognize!(tuple!(tag_no_case!("mindatetime"), OPEN, BWS, CLOSE))));
//* maxDateTimeMethodCallExpr = "maxdatetime" OPEN BWS CLOSE
named!(maxDateTimeMethodCallExpr<&str, &str>, tr!(maxDateTimeMethodCallExpr, recognize!(tuple!(tag_no_case!("maxdatetime"), OPEN, BWS, CLOSE))));
//* nowMethodCallExpr         = "now"         OPEN BWS CLOSE
named!(nowMethodCallExpr<&str, &str>, tr!(nowMethodCallExpr, recognize!(tuple!(tag_no_case!("now"), OPEN, BWS, CLOSE))));
//*
//* roundMethodCallExpr   = "round"   OPEN BWS commonExpr BWS CLOSE
named!(roundMethodCallExpr<&str, &str>, tr!(roundMethodCallExpr, recognize!(tuple!(tag_no_case!("round"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* floorMethodCallExpr   = "floor"   OPEN BWS commonExpr BWS CLOSE
named!(floorMethodCallExpr<&str, &str>, tr!(floorMethodCallExpr, recognize!(tuple!(tag_no_case!("floor"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* ceilingMethodCallExpr = "ceiling" OPEN BWS commonExpr BWS CLOSE
named!(ceilingMethodCallExpr<&str, &str>, tr!(ceilingMethodCallExpr, recognize!(tuple!(tag_no_case!("ceiling"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//*
//* distanceMethodCallExpr   = "geo.distance"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(distanceMethodCallExpr<&str, &str>, tr!(distanceMethodCallExpr, recognize!(tuple!(tag_no_case!("geo.distance"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* geoLengthMethodCallExpr  = "geo.length"     OPEN BWS commonExpr BWS CLOSE
named!(geoLengthMethodCallExpr<&str, &str>, tr!(geoLengthMethodCallExpr, recognize!(tuple!(tag_no_case!("geo.length"), OPEN, BWS, commonExpr, BWS, CLOSE))));
//* intersectsMethodCallExpr = "geo.intersects" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(intersectsMethodCallExpr<&str, &str>, tr!(intersectsMethodCallExpr, recognize!(tuple!(tag_no_case!("geo.intersects"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//*
//* hasSubsetMethodCallExpr      = "hassubset"      OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsetMethodCallExpr<&str, &str>, tr!(hasSubsetMethodCallExpr, recognize!(tuple!(tag_no_case!("hassubset"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//* hasSubsequenceMethodCallExpr = "hassubsequence" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
named!(hasSubsequenceMethodCallExpr<&str, &str>, tr!(hasSubsequenceMethodCallExpr, recognize!(tuple!(tag_no_case!("hassubsequence"), OPEN, BWS, commonExpr, BWS, COMMA, BWS, commonExpr, BWS, CLOSE))));
//*
//* parenExpr = OPEN BWS commonExpr BWS CLOSE
named!(parenExpr<&str, &str>, tr!(parenExpr, recognize!(tuple!(OPEN, BWS, commonExpr, BWS, CLOSE))));
//* listExpr  = OPEN BWS commonExpr BWS *( COMMA BWS commonExpr BWS ) CLOSE
named!(listExpr<&str, &str>, tr!(listExpr, recognize!(tuple!(OPEN, BWS, commonExpr, many0!(tuple!(COMMA, BWS, commonExpr, BWS)), CLOSE))));
//*
//* andExpr = RWS "and" RWS boolCommonExpr
named!(andExpr<&str, &str>, tr!(andExpr, recognize!(tuple!(RWS, tag_no_case!("and"), RWS, boolCommonExpr))));
//* orExpr  = RWS "or"  RWS boolCommonExpr
named!(orExpr<&str, &str>, tr!(orExpr, recognize!(tuple!(RWS, tag_no_case!("or"), RWS, boolCommonExpr))));
//*
//* eqExpr = RWS "eq" RWS commonExpr
named!(eqExpr<&str, &str>, tr!(eqExpr, recognize!(tuple!(RWS, tag_no_case!("eq"), RWS, commonExpr))));
//* neExpr = RWS "ne" RWS commonExpr
named!(neExpr<&str, &str>, tr!(neExpr, recognize!(tuple!(RWS, tag_no_case!("ne"), RWS, commonExpr))));
//* ltExpr = RWS "lt" RWS commonExpr
named!(ltExpr<&str, &str>, tr!(ltExpr, recognize!(tuple!(RWS, tag_no_case!("lt"), RWS, commonExpr))));
//* leExpr = RWS "le" RWS commonExpr
named!(leExpr<&str, &str>, tr!(leExpr, recognize!(tuple!(RWS, tag_no_case!("le"), RWS, commonExpr))));
//* gtExpr = RWS "gt" RWS commonExpr
named!(gtExpr<&str, &str>, tr!(gtExpr, recognize!(tuple!(RWS, tag_no_case!("gt"), RWS, commonExpr))));
//* geExpr = RWS "ge" RWS commonExpr
named!(geExpr<&str, &str>, tr!(geExpr, recognize!(tuple!(RWS, tag_no_case!("ge"), RWS, commonExpr))));
//* inExpr = RWS "in" RWS commonExpr
named!(inExpr<&str, &str>, tr!(inExpr, recognize!(tuple!(RWS, tag_no_case!("in"), RWS, commonExpr))));
//*
//* hasExpr = RWS "has" RWS enum
named!(hasExpr<&str, &str>, tr!(hasExpr, recognize!(tuple!(RWS, tag_no_case!("has"), RWS, commonExpr))));
//*
//* addExpr   = RWS "add"   RWS commonExpr
named!(addExpr<&str, &str>, tr!(addExpr, recognize!(tuple!(RWS, tag_no_case!("add"), RWS, commonExpr))));
//* subExpr   = RWS "sub"   RWS commonExpr
named!(subExpr<&str, &str>, tr!(subExpr, recognize!(tuple!(RWS, tag_no_case!("sub"), RWS, commonExpr))));
//* mulExpr   = RWS "mul"   RWS commonExpr
named!(mulExpr<&str, &str>, tr!(mulExpr, recognize!(tuple!(RWS, tag_no_case!("mul"), RWS, commonExpr))));
//* divExpr   = RWS "div"   RWS commonExpr
named!(divExpr<&str, &str>, tr!(divExpr, recognize!(tuple!(RWS, tag_no_case!("div"), RWS, commonExpr))));
//* divbyExpr = RWS "divby" RWS commonExpr
named!(divbyExpr<&str, &str>, tr!(divbyExpr, recognize!(tuple!(RWS, tag_no_case!("divby"), RWS, commonExpr))));
//* modExpr   = RWS "mod"   RWS commonExpr
named!(modExpr<&str, &str>, tr!(modExpr, recognize!(tuple!(RWS, tag_no_case!("mod"), RWS, commonExpr))));
//*
//* negateExpr = "-" BWS commonExpr
named!(negateExpr<&str, &str>, tr!(negateExpr, recognize!(tuple!(tag!("-"), BWS, commonExpr))));
//*
//* notExpr = "not" RWS boolCommonExpr
named!(notExpr<&str, &str>, tr!(notExpr, recognize!(tuple!(tag_no_case!("not"), RWS, boolCommonExpr))));
//*
//* isofExpr = "isof" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(isofExpr<&str, &str>, tr!(isofExpr, recognize!(tuple!(tag_no_case!("isof"), OPEN, BWS, opt!(tuple!(commonExpr, BWS, COMMA, BWS)), qualifiedTypeName, BWS, CLOSE))));
//* castExpr = "cast" OPEN BWS [ commonExpr BWS COMMA BWS ] qualifiedTypeName BWS CLOSE
named!(castExpr<&str, &str>, tr!(castExpr, recognize!(tuple!(tag_no_case!("cast"), OPEN, BWS, opt!(tuple!(commonExpr, BWS, COMMA, BWS)), qualifiedTypeName, BWS, CLOSE))));
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
named!(arrayOrObject<&str, &str>, tr!(arrayOrObject, alt!(complexColInUri | complexInUri | rootExprCol | primitiveColInUri)));
//*
//* complexColInUri = begin-array
//*                   [ complexInUri *( value-separator complexInUri ) ]
//*                   end-array
named!(complexColInUri<&str, &str>, tr!(complexColInUri, recognize!(tuple!(begin_array, opt!(tuple!(complexInUri, many0!(tuple!(value_separator, complexInUri)))), end_array))));
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
named!(complexInUri<&str, &str>, tr!(complexInUri, recognize!(tuple!(begin_object,
						   opt!(tuple!(
							       alt!(  annotationInUri
								    | primitivePropertyInUri
								    | complexPropertyInUri
								    | collectionPropertyInUri
								    | navigationPropertyInUri),
								many0!(tuple!(
									      value_separator,
									      alt!(  annotationInUri
										   | primitivePropertyInUri
										   | complexPropertyInUri
										   | collectionPropertyInUri
										   | navigationPropertyInUri)
								))
						    )),
						    end_object
						))));

//*
//* collectionPropertyInUri = ( quotation-mark primitiveColProperty quotation-mark
//*                             name-separator
//*                             primitiveColInUri
//*                           )
//*                         / ( quotation-mark complexColProperty quotation-mark
//*                             name-separator
//*                             complexColInUri
//*                           )
named!(collectionPropertyInUri<&str, &str>, tr!(collectionPropertyInUri, alt!(recognize!(tuple!(quotation_mark, primitiveColProperty, quotation_mark, name_separator, primitiveColInUri))
						 | recognize!(tuple!(quotation_mark, complexColProperty, quotation_mark, name_separator, complexColInUri)))));

//*
//* primitiveColInUri = begin-array
//*                     [ primitiveLiteralInJSON *( value-separator primitiveLiteralInJSON ) ]
//*                     end-array
named!(primitiveColInUri<&str, &str>, tr!(primitiveColInUri, recognize!(tuple!(begin_array, opt!(tuple!(primitiveLiteralInJSON, many0!(tuple!(value_separator, primitiveLiteralInJSON)))), end_array))));
//*
//* complexPropertyInUri = quotation-mark complexProperty quotation-mark
//*                        name-separator
//*                        complexInUri
named!(complexPropertyInUri<&str, &str>, tr!(complexPropertyInUri, recognize!(tuple!(quotation_mark, complexProperty, quotation_mark, name_separator, complexInUri))));
//*
//* annotationInUri = quotation-mark AT namespace "." termName quotation-mark
//*                   name-separator
//*                   ( complexInUri / complexColInUri / primitiveLiteralInJSON / primitiveColInUri )
named!(annotationInUri<&str, &str>, tr!(annotationInUri, recognize!(tuple!(quotation_mark, AT, namespace, tag!("."), termName, quotation_mark,
						      name_separator,
						      alt!(complexInUri | complexColInUri | primitiveLiteralInJSON | primitiveColInUri)))));
//*
//* primitivePropertyInUri = quotation-mark primitiveProperty quotation-mark
//*                          name-separator
//*                          primitiveLiteralInJSON
named!(primitivePropertyInUri<&str, &str>, tr!(primitivePropertyInUri, recognize!(tuple!(quotation_mark, primitiveProperty, quotation_mark, name_separator, primitiveLiteralInJSON))));
//*
//* navigationPropertyInUri = singleNavPropInJSON
//*                         / collectionNavPropInJSON
named!(navigationPropertyInUri<&str, &str>, tr!(navigationPropertyInUri, alt!(singleNavPropInJSON | collectionNavPropInJSON)));
//* singleNavPropInJSON     = quotation-mark entityNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExpr
named!(singleNavPropInJSON<&str, &str>, tr!(singleNavPropInJSON, recognize!(tuple!(quotation_mark, entityNavigationProperty, quotation_mark, name_separator, rootExpr))));
//* collectionNavPropInJSON = quotation-mark entityColNavigationProperty quotation-mark
//* 													name-separator
//* 													rootExprCol
named!(collectionNavPropInJSON<&str, &str>, tr!(collectionNavPropInJSON, recognize!(tuple!(quotation_mark, entityColNavigationProperty, quotation_mark, name_separator, rootExprCol))));
//*
//* rootExprCol = begin-array
//*               [ rootExpr *( value-separator rootExpr ) ]
//*               end-array
named!(rootExprCol<&str, &str>, tr!(rootExprCol, recognize!(tuple!(begin_array, opt!(tuple!(rootExpr, many0!(tuple!(value_separator, rootExpr)))), end_array))));
//*
//* ; JSON syntax: adapted to URI restrictions from [RFC4627]
//* begin-object = BWS ( "{" / "%7B" ) BWS
named!(begin_object<&str, &str>, tr!(begin_object, recognize!(tuple!(BWS, alt!(tag!("{") | tag!("%7B"))))));
//* end-object   = BWS ( "}" / "%7D" )
named!(end_object<&str, &str>, tr!(end_object, recognize!(tuple!(BWS, alt!(tag!("}") | tag!("%7D"))))));
//*
//* begin-array = BWS ( "[" / "%5B" ) BWS
named!(begin_array<&str, &str>, tr!(begin_array, recognize!(tuple!(BWS, alt!(tag!("[") | tag!("%5B"))))));
//* end-array   = BWS ( "]" / "%5D" )
named!(end_array<&str, &str>, tr!(end_array, recognize!(tuple!(BWS, alt!(tag!("]") | tag!("%5D"))))));
//*
//* quotation-mark  = DQUOTE / "%22"
named!(quotation_mark<&str, &str>, tr!(quotation_mark, alt!(recognize!(DQUOTE) | tag!("%22"))));
//* name-separator  = BWS COLON BWS
named!(name_separator<&str, &str>, tr!(name_separator, recognize!(tuple!(BWS, COLON, BWS))));
//* value-separator = BWS COMMA BWS
named!(value_separator<&str, &str>, tr!(value_separator, recognize!(tuple!(BWS, COMMA, BWS))));
//*
//* primitiveLiteralInJSON = stringInJSON
//*                        / numberInJSON
//*                        / 'true'
//*                        / 'false'
//*                        / 'null'
named!(primitiveLiteralInJSON<&str, &str>, tr!(primitiveLiteralInJSON, alt!(stringInJSON | numberInJSON | tag!("true") | tag!("false") | tag!("null"))));
//*
//* stringInJSON = quotation-mark *charInJSON quotation-mark
named!(stringInJSON<&str, &str>, tr!(stringInJSON, recognize!(tuple!(quotation_mark, many0!(charInJSON), quotation_mark))));
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
named!(charInJSON<&str, &str>, tr!(charInJSON, alt!(qchar_unescaped | qchar_JSON_special | recognize!(tuple!(escape, alt!(  quotation_mark
													  | escape
													  | alt!(tag!("/") | tag!("%2F"))
													  | recognize!(one_of!("bfnrt"))
													  | recognize!(tuple!(tag!("u"), many_m_n!(4, 4, HEXDIG)))
													  ))))));
//*
//* qchar-JSON-special = SP / ":" / "{" / "}" / "[" / "]" ; some agents put these unencoded into the query part of a URL
named!(qchar_JSON_special<&str, &str>, tr!(qchar_JSON_special, alt!(SP | recognize!(one_of!(":{}[]")))));
//*
//* escape = "\" / "%5C"     ; reverse solidus U+005C
named!(escape<&str, &str>, tr!(escape, alt!(tag!("\\") | tag!("%5C"))));
//*
//* numberInJSON = [ "-" ] int [ frac ] [ exp ]
named!(numberInJSON<&str, &str>, tr!(numberInJSON, recognize!(tuple!(opt!(tag!("-")), int, opt!(frac), opt!(exp)))));
//* int          = "0" / ( oneToNine *DIGIT )
named!(int<&str, &str>, tr!(int, alt!(tag!("0") | recognize!(tuple!(oneToNine, many0!(DIGIT))))));
//* frac         = "." 1*DIGIT
named!(frac<&str, &str>, tr!(frac, recognize!(tuple!(tag!("."), many1!(DIGIT)))));
//* exp          = "e" [ "-" / "+" ] 1*DIGIT
named!(exp<&str, &str>, tr!(exp, recognize!(tuple!(tag_no_case!("e"), opt!(alt!(tag!("-") | tag!("+"))), many1!(DIGIT)))));
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
named!(singleQualifiedTypeName<&str, &str>, tr!(singleQualifiedTypeName, alt!(qualifiedEntityTypeName
						 | qualifiedComplexTypeName
						 | qualifiedTypeDefinitionName
						 | qualifiedEnumTypeName
						 | primitiveTypeName)));
//*
//* qualifiedTypeName = singleQualifiedTypeName
//*                   / 'Collection' OPEN singleQualifiedTypeName CLOSE
named!(qualifiedTypeName<&str, &str>, tr!(qualifiedTypeName, alt!(singleQualifiedTypeName
					   | recognize!(tuple!(tag!("Collection"), OPEN, singleQualifiedTypeName, CLOSE)))));
//*
//* qualifiedEntityTypeName     = namespace "." entityTypeName
named!(qualifiedEntityTypeName<&str, &str>, tr!(qualifiedEntityTypeName, recognize!(tuple!(namespace, tag!("."), entityTypeName))));
//* qualifiedComplexTypeName    = namespace "." complexTypeName
named!(qualifiedComplexTypeName<&str, &str>, tr!(qualifiedComplexTypeName, recognize!(tuple!(namespace, tag!("."), complexTypeName))));
//* qualifiedTypeDefinitionName = namespace "." typeDefinitionName
named!(qualifiedTypeDefinitionName<&str, &str>, tr!(qualifiedTypeDefinitionName, recognize!(tuple!(namespace, tag!("."), typeDefinitionName))));
//* qualifiedEnumTypeName       = namespace "." enumerationTypeName
named!(qualifiedEnumTypeName<&str, &str>, tr!(qualifiedEnumTypeName, recognize!(tuple!(namespace, tag!("."), enumerationTypeName))));
//*
//* ; an alias is just a single-part namespace
//* namespace     = namespacePart *( "." namespacePart )
named!(namespace<&str, &str>, tr!(namespace, recognize!(tuple!(namespacePart, many0!(tuple!(tag!("."), namespacePart))))));
//* namespacePart = odataIdentifier
named!(namespacePart<&str, &str>, tr!(namespacePart, recognize!(odataIdentifier)));
//*
//* entitySetName       = odataIdentifier
named!(entitySetName<&str, &str>, tr!(entitySetName, recognize!(odataIdentifier)));
//* singletonEntity     = odataIdentifier
named!(singletonEntity<&str, &str>, tr!(singletonEntity, recognize!(odataIdentifier)));
//* entityTypeName      = odataIdentifier
named!(entityTypeName<&str, &str>, tr!(entityTypeName, recognize!(odataIdentifier)));
//* complexTypeName     = odataIdentifier
named!(complexTypeName<&str, &str>, tr!(complexTypeName, recognize!(odataIdentifier)));
//* typeDefinitionName  = odataIdentifier
named!(typeDefinitionName<&str, &str>, tr!(typeDefinitionName, recognize!(odataIdentifier)));
//* enumerationTypeName = odataIdentifier
named!(enumerationTypeName<&str, &str>, tr!(enumerationTypeName, recognize!(odataIdentifier)));
//* enumerationMember   = odataIdentifier
named!(enumerationMember<&str, &str>, tr!(enumerationMember, recognize!(odataIdentifier)));
//* termName            = odataIdentifier
named!(termName<&str, &str>, tr!(termName, recognize!(odataIdentifier)));

//TODO(restrictive + unicode)
//* ; Note: this pattern is overly restrictive, the normative definition is type TSimpleIdentifier in OData EDM XML Schema
//* odataIdentifier             = identifierLeadingCharacter *127identifierCharacter
named!(odataIdentifier<&str, &str>, tr!(odataIdentifier, recognize!(tuple!(identifierLeadingCharacter, many_m_n!(0, 127, identifierCharacter)))));
//* identifierLeadingCharacter  = ALPHA / "_"         ; plus Unicode characters from the categories L or Nl
named!(identifierLeadingCharacter<&str, &str>, tr!(identifierLeadingCharacter, alt!(ALPHA | tag!("_"))));
//* identifierCharacter         = ALPHA / "_" / DIGIT ; plus Unicode characters from the categories L, Nl, Nd, Mn, Mc, Pc, or Cf
named!(identifierCharacter<&str, &str>, tr!(identifierCharacter, alt!(ALPHA | tag!("_") | DIGIT)));
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
named!(primitiveTypeName<&str, &str>, tr!(primitiveTypeName, recognize!(tuple!(tag!("Edm."), alt!(  tag!("Binary")
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
									)))));
//* abstractSpatialTypeName = 'Geography'
//*                         / 'Geometry'
named!(abstractSpatialTypeName<&str, &str>, tr!(abstractSpatialTypeName, alt!(tag!("Geography") | tag!("Geometry"))));
//* concreteSpatialTypeName = 'Collection'
//*                         / 'LineString'
//*                         / 'MultiLineString'
//*                         / 'MultiPoint'
//*                         / 'MultiPolygon'
//*                         / 'Point'
//*                         / 'Polygon'
named!(concreteSpatialTypeName<&str, &str>, tr!(concreteSpatialTypeName, alt!(  tag!("Collection")
						 | tag!("LineString")
						 | tag!("MultiLineString")
						 | tag!("MultiPoint")
						 | tag!("MultiPolygon")
						 | tag!("Point")
						 | tag!("Polygon")
						 )));
//*
//* primitiveProperty       = primitiveKeyProperty / primitiveNonKeyProperty
named!(primitiveProperty<&str, &str>, tr!(primitiveProperty, alt!(primitiveKeyProperty | primitiveNonKeyProperty)));
//* primitiveKeyProperty    = odataIdentifier
named!(primitiveKeyProperty<&str, &str>, tr!(primitiveKeyProperty, recognize!(odataIdentifier)));
//* primitiveNonKeyProperty = odataIdentifier
named!(primitiveNonKeyProperty<&str, &str>, tr!(primitiveNonKeyProperty, recognize!(odataIdentifier)));
//* primitiveColProperty    = odataIdentifier
named!(primitiveColProperty<&str, &str>, tr!(primitiveColProperty, recognize!(odataIdentifier)));
//* complexProperty         = odataIdentifier
named!(complexProperty<&str, &str>, tr!(complexProperty, recognize!(odataIdentifier)));
//* complexColProperty      = odataIdentifier
named!(complexColProperty<&str, &str>, tr!(complexColProperty, recognize!(odataIdentifier)));
//* streamProperty          = odataIdentifier
named!(streamProperty<&str, &str>, tr!(streamProperty, recognize!(odataIdentifier)));
//*
//* navigationProperty          = entityNavigationProperty / entityColNavigationProperty
named!(navigationProperty<&str, &str>, tr!(navigationProperty, alt!(entityNavigationProperty | entityColNavigationProperty)));
//* entityNavigationProperty    = odataIdentifier
named!(entityNavigationProperty<&str, &str>, tr!(entityNavigationProperty, recognize!(odataIdentifier)));
//* entityColNavigationProperty = odataIdentifier
named!(entityColNavigationProperty<&str, &str>, tr!(entityColNavigationProperty, recognize!(odataIdentifier)));
//*
//* action       = odataIdentifier
named!(action<&str, &str>, tr!(action, recognize!(odataIdentifier)));
//* actionImport = odataIdentifier
named!(actionImport<&str, &str>, tr!(actionImport, recognize!(odataIdentifier)));
//*
//* function = entityFunction
//*          / entityColFunction
//*          / complexFunction
//*          / complexColFunction
//*          / primitiveFunction
//*          / primitiveColFunction
named!(function<&str, &str>, tr!(function, alt!( entityFunction
				 | entityColFunction
				 | complexFunction
				 | complexColFunction
				 | primitiveFunction
				 | primitiveColFunction)));
//*
//* entityFunction       = odataIdentifier
named!(entityFunction<&str, &str>, tr!(entityFunction, recognize!(odataIdentifier)));
//* entityColFunction    = odataIdentifier
named!(entityColFunction<&str, &str>, tr!(entityColFunction, recognize!(odataIdentifier)));
//* complexFunction      = odataIdentifier
named!(complexFunction<&str, &str>, tr!(complexFunction, recognize!(odataIdentifier)));
//* complexColFunction   = odataIdentifier
named!(complexColFunction<&str, &str>, tr!(complexColFunction, recognize!(odataIdentifier)));
//* primitiveFunction    = odataIdentifier
named!(primitiveFunction<&str, &str>, tr!(primitiveFunction, recognize!(odataIdentifier)));
//* primitiveColFunction = odataIdentifier
named!(primitiveColFunction<&str, &str>, tr!(primitiveColFunction, recognize!(odataIdentifier)));
//*
//* entityFunctionImport       = odataIdentifier
named!(entityFunctionImport<&str, &str>, tr!(entityFunctionImport, recognize!(odataIdentifier)));
//* entityColFunctionImport    = odataIdentifier
named!(entityColFunctionImport<&str, &str>, tr!(entityColFunctionImport, recognize!(odataIdentifier)));
//* complexFunctionImport      = odataIdentifier
named!(complexFunctionImport<&str, &str>, tr!(complexFunctionImport, recognize!(odataIdentifier)));
//* complexColFunctionImport   = odataIdentifier
named!(complexColFunctionImport<&str, &str>, tr!(complexColFunctionImport, recognize!(odataIdentifier)));
//* primitiveFunctionImport    = odataIdentifier
named!(primitiveFunctionImport<&str, &str>, tr!(primitiveFunctionImport, recognize!(odataIdentifier)));
//* primitiveColFunctionImport = odataIdentifier
named!(primitiveColFunctionImport<&str, &str>, tr!(primitiveColFunctionImport, recognize!(odataIdentifier)));
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
named!(primitiveLiteral<&str, &str>, tr!(primitiveLiteral, alt!(  nullValue
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
					  | geometryPolygon)));
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
named!(primitiveValue<&str, &str>, tr!(primitiveValue, alt!( booleanValue
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
				       | binaryValue)));
//*
//* nullValue = 'null'
named!(nullValue<&str, &str>, tr!(nullValue, tag!("null")));
//*
//* ; base64url encoding according to http://tools.ietf.org/html/rfc4648#section-5
//* binary      = "binary" SQUOTE binaryValue SQUOTE
named!(binary<&str, &str>, tr!(binary, recognize!(tuple!(tag_no_case!("binary"), SQUOTE, binaryValue, SQUOTE))));
//* binaryValue = *(4base64char) [ base64b16  / base64b8 ]
named!(binaryValue<&str, &str>, tr!(binaryValue, recognize!(tuple!(many0!(many_m_n!(4, 4, base64char)), opt!(alt!(base64b16 | base64b8))))));
//* base64b16   = 2base64char ( 'A' / 'E' / 'I' / 'M' / 'Q' / 'U' / 'Y' / 'c' / 'g' / 'k' / 'o' / 's' / 'w' / '0' / '4' / '8' )   [ "=" ]
named!(base64b16<&str, &str>, tr!(base64b16, recognize!(tuple!(many_m_n!(2, 2, base64char), one_of!("AEIMQUYcgkosw048"), opt!(tag!("="))))));
//* base64b8    = base64char ( 'A' / 'Q' / 'g' / 'w' ) [ "==" ]
named!(base64b8<&str, &str>, tr!(base64b8, recognize!(tuple!(base64char, one_of!("AQgw"), opt!(tag!("=="))))));
//* base64char  = ALPHA / DIGIT / "-" / "_"
named!(base64char<&str, &str>, tr!(base64char, alt!(ALPHA | DIGIT | tag!("-") | tag!("_"))));
//*
//* booleanValue = "true" / "false"
named!(booleanValue<&str, &str>, tr!(booleanValue, alt!(tag_no_case!("true") | tag_no_case!("false"))));
//*
//* decimalValue = [ SIGN ] 1*DIGIT [ "." 1*DIGIT ] [ "e" [ SIGN ] 1*DIGIT ] / nanInfinity
named!(decimalValue<&str, &str>, tr!(decimalValue, alt!(recognize!(tuple!(opt!(SIGN),
							many1!(DIGIT),
						   	opt!(tuple!(tag!("."), many1!(DIGIT))),
						   	opt!(tuple!(tag_no_case!("e"), opt!(SIGN), many1!(DIGIT)))
							))
				      | nanInfinity)));
//* doubleValue  = decimalValue ; IEEE 754 binary64 floating-point number (15-17 decimal digits)
named!(doubleValue<&str, &str>, tr!(doubleValue, recognize!(decimalValue)));
//* singleValue  = decimalValue ; IEEE 754 binary32 floating-point number (6-9 decimal digits)
named!(singleValue<&str, &str>, tr!(singleValue, recognize!(decimalValue)));
//* nanInfinity  = 'NaN' / '-INF' / 'INF'
named!(nanInfinity<&str, &str>, tr!(nanInfinity, alt!(tag!("NaN") | tag!("-INF") | tag!("INF"))));
//*
//* guidValue = 8HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 4HEXDIG "-" 12HEXDIG
named!(guidValue<&str, &str>, tr!(guidValue, recognize!(tuple!(many_m_n!(8, 8, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(4, 4, HEXDIG), tag!("-"),
						many_m_n!(12, 12, HEXDIG)
						))));
//*
//* byteValue  = 1*3DIGIT           ; numbers in the range from 0 to 255
named!(byteValue<&str, &str>, tr!(byteValue, recognize!(many_m_n!(1, 3, DIGIT))));
//* sbyteValue = [ SIGN ] 1*3DIGIT  ; numbers in the range from -128 to 127
named!(sbyteValue<&str, &str>, tr!(sbyteValue, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 3, DIGIT)))));
//* int16Value = [ SIGN ] 1*5DIGIT  ; numbers in the range from -32768 to 32767
named!(int16Value<&str, &str>, tr!(int16Value, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 5, DIGIT)))));
//* int32Value = [ SIGN ] 1*10DIGIT ; numbers in the range from -2147483648 to 2147483647
named!(int32Value<&str, &str>, tr!(int32Value, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 10, DIGIT)))));
//* int64Value = [ SIGN ] 1*19DIGIT ; numbers in the range from -9223372036854775808 to 9223372036854775807
named!(int64Value<&str, &str>, tr!(int64Value, recognize!(tuple!(opt!(SIGN), many_m_n!(1, 19, DIGIT)))));
//*
//* string           = SQUOTE *( SQUOTE-in-string / pchar-no-SQUOTE ) SQUOTE
named!(string<&str, &str>, tr!(string, recognize!(tuple!(SQUOTE, many0!(alt!(recognize!(SQUOTE_in_string) | recognize!(pchar_no_SQUOTE))), SQUOTE))));
//* SQUOTE-in-string = SQUOTE SQUOTE ; two consecutive single quotes represent one within a string literal
named!(SQUOTE_in_string<&str, &str>, tr!(SQUOTE_in_string, recognize!(tuple!(SQUOTE, SQUOTE))));
//*
//* dateValue = year "-" month "-" day
named!(dateValue<&str, &str>, tr!(dateValue, recognize!(tuple!(year, tag!("-"), month, tag!("-"), day))));
//*
//* dateTimeOffsetValue = year "-" month "-" day "T" hour ":" minute [ ":" second [ "." fractionalSeconds ] ] ( "Z" / SIGN hour ":" minute )
named!(dateTimeOffsetValue<&str, &str>, tr!(dateTimeOffsetValue, recognize!(tuple!(year, tag!("-"), month, tag!("-"), day, tag_no_case!("T"), hour, tag!(":"), minute,
							  opt!(tuple!(tag!(":"), second, opt!(tuple!(tag!("."), fractionalSeconds)))),
							  alt!(tag_no_case!("Z") | recognize!(tuple!(SIGN, hour, tag!(":"), minute)))
							  ))));
//*
//* duration      = [ "duration" ] SQUOTE durationValue SQUOTE
named!(duration<&str, &str>, tr!(duration, recognize!(tuple!(opt!(tag_no_case!("duration")), SQUOTE, durationValue, SQUOTE))));
//* durationValue = [ SIGN ] "P" [ 1*DIGIT "D" ] [ "T" [ 1*DIGIT "H" ] [ 1*DIGIT "M" ] [ 1*DIGIT [ "." 1*DIGIT ] "S" ] ]
//*      ; the above is an approximation of the rules for an xml dayTimeDuration.
//*      ; see the lexical representation for dayTimeDuration in http://www.w3.org/TR/xmlschema11-2#dayTimeDuration for more information
named!(durationValue<&str, &str>, tr!(durationValue, recognize!(tuple!(opt!(SIGN),
						    tag_no_case!("P"),
						    opt!(tuple!(many1!(DIGIT), tag_no_case!("D"))),
						    opt!(tuple!(tag_no_case!("T"),
								opt!(tuple!(many1!(DIGIT), tag_no_case!("H"))),
								opt!(tuple!(many1!(DIGIT), tag_no_case!("M"))),
								opt!(tuple!(many1!(DIGIT), opt!(tuple!(tag!("."), many1!(DIGIT))), tag_no_case!("S")))
								))))));

//*
//* timeOfDayValue = hour ":" minute [ ":" second [ "." fractionalSeconds ] ]
named!(timeOfDayValue<&str, &str>, tr!(timeOfDayValue, recognize!(tuple!(hour, tag!(":"), minute, opt!(tuple!(tag!(":"), second, opt!(tuple!(tag!("."), fractionalSeconds))))))));
//*
//* oneToNine       = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
named!(oneToNine<&str, &str>, tr!(oneToNine, recognize!(one_of!("123456789"))));
//* zeroToFiftyNine = ( "0" / "1" / "2" / "3" / "4" / "5" ) DIGIT
named!(zeroToFiftyNine<&str, &str>, tr!(zeroToFiftyNine, recognize!(tuple!(one_of!("012345"), DIGIT))));
//* year  = [ "-" ] ( "0" 3DIGIT / oneToNine 3*DIGIT )
named!(year<&str, &str>, tr!(year, recognize!(tuple!(opt!(tag!("-")), alt!(recognize!(tuple!(tag!("0"), many_m_n!(3, 3, DIGIT))) | recognize!(tuple!(oneToNine, many_m_n!(3, 3, DIGIT))))))));
//* month = "0" oneToNine
//*       / "1" ( "0" / "1" / "2" )
named!(month<&str, &str>, tr!(month, alt!(  recognize!(tuple!(tag!("0"), oneToNine))
			       | recognize!(tuple!(tag!("1"), one_of!("012")))
			       )));
//* day   = "0" oneToNine
//*       / ( "1" / "2" ) DIGIT
//*       / "3" ( "0" / "1" )
named!(day<&str, &str>, tr!(day, alt!(  recognize!(tuple!(tag!("0"), oneToNine))
			     | recognize!(tuple!(one_of!("12"), DIGIT))
			     | recognize!(tuple!(tag!("3"), one_of!("01")))
			     )));
//* hour   = ( "0" / "1" ) DIGIT
//*        / "2" ( "0" / "1" / "2" / "3" )
named!(hour<&str, &str>, tr!(hour, alt!(  recognize!(tuple!(one_of!("01"), DIGIT))
			      | recognize!(tuple!(tag!("2"), one_of!("0123")))
			      )));
//* minute = zeroToFiftyNine
named!(minute<&str, &str>, tr!(minute, recognize!(zeroToFiftyNine)));
//* second = zeroToFiftyNine
named!(second<&str, &str>, tr!(second, recognize!(zeroToFiftyNine)));
//* fractionalSeconds = 1*12DIGIT
named!(fractionalSeconds<&str, &str>, tr!(fractionalSeconds, recognize!(many_m_n!(1, 12, DIGIT))));
//*
//* enum            = [ qualifiedEnumTypeName ] SQUOTE enumValue SQUOTE
named!(_enum<&str, &str>, tr!(_enum, recognize!(tuple!(opt!(qualifiedEntityTypeName), SQUOTE, enumValue, SQUOTE))));
//* enumValue       = singleEnumValue *( COMMA singleEnumValue )
named!(enumValue<&str, &str>, tr!(enumValue, recognize!(tuple!(singleEnumValue, many0!(tuple!(COMMA, singleEnumValue))))));
//* singleEnumValue = enumerationMember / enumMemberValue
named!(singleEnumValue<&str, &str>, tr!(singleEnumValue, alt!(enumerationMember | enumMemberValue)));
//* enumMemberValue = int64Value
named!(enumMemberValue<&str, &str>, tr!(enumMemberValue, recognize!(int64Value)));


//* geographyCollection   = geographyPrefix SQUOTE fullCollectionLiteral SQUOTE
named!(geographyCollection<&str, &str>, tr!(geographyCollection, recognize!(tuple!(geographyPrefix, SQUOTE, fullCollectionLiteral, SQUOTE))));
//* fullCollectionLiteral = sridLiteral collectionLiteral
named!(fullCollectionLiteral<&str, &str>, tr!(fullCollectionLiteral, recognize!(tuple!(sridLiteral, collectionLiteral))));
//* collectionLiteral     = "Collection(" geoLiteral *( COMMA geoLiteral ) CLOSE
named!(collectionLiteral<&str, &str>, tr!(collectionLiteral, recognize!(tuple!(tag_no_case!("Collection("), geoLiteral, many0!(tuple!(COMMA, geoLiteral)), CLOSE))));
//* geoLiteral            = collectionLiteral
//*                       / lineStringLiteral
//*                       / multiPointLiteral
//*                       / multiLineStringLiteral
//*                       / multiPolygonLiteral
//*                       / pointLiteral
//*                       / polygonLiteral
named!(geoLiteral<&str, &str>, tr!(geoLiteral, alt!(  collectionLiteral
				    | lineStringLiteral
				    | multiPointLiteral
				    | multiLineStringLiteral
				    | multiPolygonLiteral
				    | pointLiteral
				    | polygonLiteral)));
//*
//* geographyLineString   = geographyPrefix SQUOTE fullLineStringLiteral SQUOTE
named!(geographyLineString<&str, &str>, tr!(geographyLineString, recognize!(tuple!(geographyPrefix, SQUOTE, fullLineStringLiteral, SQUOTE))));
//* fullLineStringLiteral = sridLiteral lineStringLiteral
named!(fullLineStringLiteral<&str, &str>, tr!(fullLineStringLiteral, recognize!(tuple!(sridLiteral, lineStringLiteral))));
//* lineStringLiteral     = "LineString" lineStringData
named!(lineStringLiteral<&str, &str>, tr!(lineStringLiteral, recognize!(tuple!(tag_no_case!("LineString"), lineStringData))));
//* lineStringData        = OPEN positionLiteral 1*( COMMA positionLiteral ) CLOSE
named!(lineStringData<&str, &str>, tr!(lineStringData, recognize!(tuple!(OPEN, positionLiteral, many1!(tuple!(COMMA, positionLiteral)), CLOSE))));
//*
//* geographyMultiLineString   = geographyPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geographyMultiLineString<&str, &str>, tr!(geographyMultiLineString, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE))));
//* fullMultiLineStringLiteral = sridLiteral multiLineStringLiteral
named!(fullMultiLineStringLiteral<&str, &str>, tr!(fullMultiLineStringLiteral, recognize!(tuple!(sridLiteral, multiLineStringLiteral))));
//* multiLineStringLiteral     = "MultiLineString(" [ lineStringData *( COMMA lineStringData ) ] CLOSE
named!(multiLineStringLiteral<&str, &str>, tr!(multiLineStringLiteral, recognize!(tuple!(tag_no_case!("MultiLineString("), opt!(tuple!(lineStringData, many0!(tuple!(COMMA, lineStringData)))), CLOSE))));
//*
//* geographyMultiPoint   = geographyPrefix SQUOTE fullMultiPointLiteral SQUOTE
named!(geographyMultiPoint<&str, &str>, tr!(geographyMultiPoint, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE))));
//* fullMultiPointLiteral = sridLiteral multiPointLiteral
named!(fullMultiPointLiteral<&str, &str>, tr!(fullMultiPointLiteral, recognize!(tuple!(sridLiteral, multiPointLiteral))));
//* multiPointLiteral     = "MultiPoint(" [ pointData *( COMMA pointData ) ] CLOSE
named!(multiPointLiteral<&str, &str>, tr!(multiPointLiteral, recognize!(tuple!(tag_no_case!("MultiPoint("), opt!(tuple!(pointData, many0!(tuple!(COMMA, pointData)))), CLOSE))));
//*
//* geographyMultiPolygon   = geographyPrefix SQUOTE fullMultiPolygonLiteral SQUOTE
named!(geographyMultiPolygon<&str, &str>, tr!(geographyMultiPolygon, recognize!(tuple!(geographyPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE))));
//* fullMultiPolygonLiteral = sridLiteral multiPolygonLiteral
named!(fullMultiPolygonLiteral<&str, &str>, tr!(fullMultiPolygonLiteral, recognize!(tuple!(sridLiteral, multiPolygonLiteral))));
//* multiPolygonLiteral     = "MultiPolygon(" [ polygonData *( COMMA polygonData ) ] CLOSE
named!(multiPolygonLiteral<&str, &str>, tr!(multiPolygonLiteral, recognize!(tuple!(tag_no_case!("MultiPolygon("), opt!(tuple!(polygonData, many0!(tuple!(COMMA, polygonData)))), CLOSE))));
//*
//* geographyPoint   = geographyPrefix SQUOTE fullPointLiteral SQUOTE
named!(geographyPoint<&str, &str>, tr!(geographyPoint, recognize!(tuple!(geographyPrefix, SQUOTE, fullPointLiteral, SQUOTE))));
//* fullPointLiteral = sridLiteral pointLiteral
named!(fullPointLiteral<&str, &str>, tr!(fullPointLiteral, recognize!(tuple!(sridLiteral, pointLiteral))));
//* sridLiteral      = "SRID" EQ 1*5DIGIT SEMI
named!(sridLiteral<&str, &str>, tr!(sridLiteral, recognize!(tuple!(tag_no_case!("SRID"), EQ, many_m_n!(1, 5, DIGIT), SEMI))));
//* pointLiteral     ="Point" pointData
named!(pointLiteral<&str, &str>, tr!(pointLiteral, recognize!(tuple!(tag_no_case!("Point"), pointData))));
//* pointData        = OPEN positionLiteral CLOSE
named!(pointData<&str, &str>, tr!(pointData, recognize!(tuple!(OPEN, positionLiteral, CLOSE))));
//* positionLiteral  = doubleValue SP doubleValue  ; longitude, then latitude
named!(positionLiteral<&str, &str>, tr!(positionLiteral, recognize!(tuple!(doubleValue, SP, doubleValue))));
//*
//* geographyPolygon   = geographyPrefix SQUOTE fullPolygonLiteral SQUOTE
named!(geographyPolygon<&str, &str>, tr!(geographyPolygon, recognize!(tuple!(geographyPrefix, SQUOTE, fullPolygonLiteral, SQUOTE))));
//* fullPolygonLiteral = sridLiteral polygonLiteral
named!(fullPolygonLiteral<&str, &str>, tr!(fullPolygonLiteral, recognize!(tuple!(sridLiteral, polygonLiteral))));
//* polygonLiteral     = "Polygon" polygonData
named!(polygonLiteral<&str, &str>, tr!(polygonLiteral, recognize!(tuple!(tag_no_case!("Polygon"), polygonData))));
//* polygonData        = OPEN ringLiteral *( COMMA ringLiteral ) CLOSE
named!(polygonData<&str, &str>, tr!(polygonData, recognize!(tuple!(OPEN, ringLiteral, many0!(tuple!(COMMA, ringLiteral)), CLOSE))));
//* ringLiteral        = OPEN positionLiteral *( COMMA positionLiteral ) CLOSE
//*                    ; Within each ringLiteral, the first and last positionLiteral elements MUST be an exact syntactic match to each other.
//*                    ; Within the polygonData, the ringLiterals MUST specify their points in appropriate winding order.
//*                    ; In order of traversal, points to the left side of the ring are interpreted as being in the polygon.
named!(ringLiteral<&str, &str>, tr!(ringLiteral, recognize!(tuple!(OPEN, positionLiteral, many0!(tuple!(COMMA, positionLiteral)), CLOSE))));
//*
//* geometryCollection      = geometryPrefix SQUOTE fullCollectionLiteral      SQUOTE
named!(geometryCollection<&str, &str>, tr!(geometryCollection, recognize!(tuple!(geometryPrefix, SQUOTE, fullCollectionLiteral, SQUOTE))));
//* geometryLineString      = geometryPrefix SQUOTE fullLineStringLiteral      SQUOTE
named!(geometryLineString<&str, &str>, tr!(geometryLineString, recognize!(tuple!(geometryPrefix, SQUOTE, fullLineStringLiteral, SQUOTE))));
//* geometryMultiLineString = geometryPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
named!(geometryMultiLineString<&str, &str>, tr!(geometryMultiLineString, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiLineStringLiteral, SQUOTE))));
//* geometryMultiPoint      = geometryPrefix SQUOTE fullMultiPointLiteral      SQUOTE
named!(geometryMultiPoint<&str, &str>, tr!(geometryMultiPoint, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiPointLiteral, SQUOTE))));
//* geometryMultiPolygon    = geometryPrefix SQUOTE fullMultiPolygonLiteral    SQUOTE
named!(geometryMultiPolygon<&str, &str>, tr!(geometryMultiPolygon, recognize!(tuple!(geometryPrefix, SQUOTE, fullMultiPolygonLiteral, SQUOTE))));
//* geometryPoint           = geometryPrefix SQUOTE fullPointLiteral           SQUOTE
named!(geometryPoint<&str, &str>, tr!(geometryPoint, recognize!(tuple!(geometryPrefix, SQUOTE, fullPointLiteral, SQUOTE))));
//* geometryPolygon         = geometryPrefix SQUOTE fullPolygonLiteral         SQUOTE
named!(geometryPolygon<&str, &str>, tr!(geometryPolygon, recognize!(tuple!(geometryPrefix, SQUOTE, fullPolygonLiteral, SQUOTE))));
//*
//* geographyPrefix = "geography"
named!(geographyPrefix<&str, &str>, tr!(geographyPrefix, tag_no_case!("geography")));
//* geometryPrefix  = "geometry"
named!(geometryPrefix<&str, &str>, tr!(geometryPrefix, tag_no_case!("geometry")));
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
named!(obs_text<&str, &str>, tr!(obs_text, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && (chr as u8) >= 0x80))));
//* ;quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
//*
//* OWS   = *( SP / HTAB )  ; "optional" whitespace
named!(OWS<&str, &str>, tr!(OWS, recognize!(many0!(alt!(SP | HTAB)))));
//* BWS-h = *( SP / HTAB )  ; "bad" whitespace in header values
named!(BWS_h<&str, &str>, tr!(BWS_h, recognize!(many0!(alt!(SP | HTAB)))));
//* EQ-h  = BWS-h EQ BWS-h
named!(EQ_h<&str, &str>, tr!(EQ_h, recognize!(tuple!(BWS_h, EQ, BWS_h))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; 9. Punctuation
//* ;------------------------------------------------------------------------------
//*
//* RWS = 1*( SP / HTAB / "%20" / "%09" )  ; "required" whitespace
named!(RWS<&str, &str>, tr!(RWS, recognize!(many1!(alt!(SP | HTAB | tag!("%20") | tag!("%09"))))));
//* BWS =  *( SP / HTAB / "%20" / "%09" )  ; "bad" whitespace
named!(BWS<&str, &str>, tr!(BWS, recognize!(many0!(alt!(SP | HTAB | tag!("%20") | tag!("%09"))))));
//*
//* AT     = "@" / "%40"
named!(AT<&str, &str>, tr!(AT, alt!(tag!("@") | tag!("%40"))));
//* COLON  = ":" / "%3A"
named!(COLON<&str, &str>, tr!(COLON, alt!(tag!(":") | tag!("%3A"))));
//* COMMA  = "," / "%2C"
named!(COMMA<&str, &str>, tr!(COMMA, alt!(tag!(",") | tag!("%2C"))));
//* EQ     = "="
named!(EQ<&str, &str>, tr!(EQ, tag!("=")));
//* SIGN   = "+" / "%2B" / "-"
named!(SIGN<&str, &str>, tr!(SIGN, alt!(tag!("+") | tag!("%3B") | tag!("-"))));
//* SEMI   = ";" / "%3B"
named!(SEMI<&str, &str>, tr!(SEMI, alt!(tag!(";") | tag!("%3B"))));
//* STAR   = "*" / "%2A"
named!(STAR<&str, &str>, tr!(STAR, alt!(tag!("*") | tag!("%2A"))));
//* SQUOTE = "'" / "%27"
named!(SQUOTE<&str, &str>, tr!(SQUOTE, alt!(tag!("'") | tag!("%27"))));
//*
//* OPEN  = "(" / "%28"
named!(OPEN<&str, &str>, tr!(OPEN, alt!(tag!("(") | tag!("%28"))));
//* CLOSE = ")" / "%29"
named!(CLOSE<&str, &str>, tr!(CLOSE, alt!(tag!(")") | tag!("%29"))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; A. URI syntax [RFC3986]
//* ;------------------------------------------------------------------------------
//*
//* URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
named!(URI<&str, &str>, tr!(URI, recognize!(tuple!(scheme, tag!(":"), hier_part, opt!(tuple!(tag!("?"), query)), opt!(tuple!(tag!("#"), fragment))))));
//* hier-part     = "//" authority path-abempty
//*               / path-absolute
//*               / path-rootless
//* ;              / path-empty
named!(hier_part<&str, &str>, tr!(hier_part, recognize!(tuple!(tag!("//"), authority, alt!(path_abempty | path_absolute | path_rootless | path_empty)))));
//* ;URI-reference = URI / relative-ref
//* ;absolute-URI  = scheme ":" hier-part [ "?" query ]
//* ;relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
//* ;relative-part = "//" authority path-abempty
//* ;              / path-absolute
//* ;              / path-noscheme
//* ;              / path-empty
//* scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
named!(scheme<&str, &str>, tr!(scheme, recognize!(tuple!(ALPHA, many0!(alt!(ALPHA | DIGIT | tag!("+") | tag!("-") | tag!(".")))))));
//* authority     = [ userinfo "@" ] host [ ":" port ]
named!(authority<&str, &str>, tr!(authority, recognize!(tuple!(opt!(tuple!(userinfo, tag!("@"))), host, opt!(tuple!(tag!(":"), port))))));
//* userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
named!(userinfo<&str, &str>, tr!(userinfo, recognize!(many0!(alt!(unreserved | pct_encoded | sub_delims | tag!(":"))))));
//* host          = IP-literal / IPv4address / reg-name
named!(host<&str, &str>, tr!(host, alt!(IP_literal | IPv4address | reg_name)));
//* port          = *DIGIT
named!(port<&str, &str>, tr!(port, recognize!(many0!(DIGIT))));
//* IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
named!(IP_literal<&str, &str>, tr!(IP_literal, delimited!(tag!("["), alt!(IPv6address | IPvFuture), tag!("]"))));
//* IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
named!(IPvFuture<&str, &str>, tr!(IPvFuture, recognize!(tuple!(tag!("v"), many1!(HEXDIG), tag!("."), many1!(alt!(unreserved | sub_delims | tag!(":")))))));
//* IPv6address   =                            6( h16 ":" ) ls32
//*                  /                       "::" 5( h16 ":" ) ls32
//*                  / [               h16 ] "::" 4( h16 ":" ) ls32
//*                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
//*                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
//*                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
//*                  / [ *4( h16 ":" ) h16 ] "::"              ls32
//*                  / [ *5( h16 ":" ) h16 ] "::"              h16
//*                  / [ *6( h16 ":" ) h16 ] "::"
named!(IPv6address<&str, &str>, tr!(IPv6address, alt!(
		recognize!(tuple!(                                                                        many_m_n!(6, 6, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(                                                            tag!("::"), many_m_n!(5, 5, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(                                                h16 ), tag!("::"), many_m_n!(4, 4, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 1, tuple!(h16, tag!(":"))), h16)), tag!("::"), many_m_n!(3, 3, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 2, tuple!(h16, tag!(":"))), h16)), tag!("::"), many_m_n!(2, 2, tuple!(h16, tag!(":"))), ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 3, tuple!(h16, tag!(":"))), h16)), tag!("::"),                        h16, tag!(":"),   ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 4, tuple!(h16, tag!(":"))), h16)), tag!("::"),                                          ls32)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 5, tuple!(h16, tag!(":"))), h16)), tag!("::"),                                           h16)) |
		recognize!(tuple!(opt!(tuple!(many_m_n!(0, 6, tuple!(h16, tag!(":"))), h16)), tag!("::")                                               ))
)));
//* h16           = 1*4HEXDIG
named!(h16<&str, &str>, tr!(h16, recognize!(many1!(many_m_n!(4, 4, HEXDIG)))));
//* ls32          = ( h16 ":" h16 ) / IPv4address
named!(ls32<&str, &str>, tr!(ls32, alt!(recognize!(separated_pair!(h16, tag!(":"), h16)) | IPv4address)));
//* IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
named!(IPv4address<&str, &str>, tr!(IPv4address, recognize!(tuple!(dec_octet, tag!("."), dec_octet, tag!("."), dec_octet, tag!("."), dec_octet))));
//* dec-octet     = "1" 2DIGIT            ; 100-199
//*               / "2" %x30-34 DIGIT     ; 200-249
//*               / "25" %x30-35          ; 250-255
//*               / %x31-39 DIGIT         ; 10-99
//*               / DIGIT                 ; 0-9
named!(dec_octet<&str, &str>, tr!(dec_octet, alt!(
	recognize!(tuple!(tag!("1"), DIGIT, DIGIT))            |
	recognize!(tuple!(tag!("2"), one_of!("01234"), DIGIT)) |
	recognize!(tuple!(tag!("25"), one_of!("012345")))      |
	recognize!(tuple!(one_of!("123456789"), DIGIT))        |
	DIGIT
)));
//* reg-name      = *( unreserved / pct-encoded / sub-delims )
named!(reg_name<&str, &str>, tr!(reg_name, recognize!(many0!(alt!(unreserved | pct_encoded | sub_delims)))));
//* ;path          = path-abempty    ; begins with "/" or is empty
//* ;              / path-absolute   ; begins with "/" but not "//"
//* ;              / path-noscheme   ; begins with a non-colon segment
//* ;              / path-rootless   ; begins with a segment
//* ;              / path-empty      ; zero characters
//* path-abempty  = *( "/" segment )
named!(path_abempty<&str, &str>, tr!(path_abempty, recognize!(many0!(preceded!(tag!("/"), segment)))));
//* path-absolute = "/" [ segment-nz *( "/" segment ) ]
named!(path_absolute<&str, &str>, tr!(path_absolute, recognize!(tuple!(tag!("/"), opt!(tuple!(segment_nz, path_abempty))))));
//* ;path-noscheme = segment-nz-nc *( "/" segment )
//* path-rootless = segment-nz *( "/" segment )
named!(path_rootless<&str, &str>, tr!(path_rootless, recognize!(tuple!(segment_nz, path_abempty))));
//* ;path-empty    = ""
named!(path_empty<&str, &str>, tr!(path_empty, tag!("")));
//* segment       = *pchar
named!(segment<&str, &str>, tr!(segment, recognize!(many0!(pchar))));
//* segment-nz    = 1*pchar
named!(segment_nz<&str, &str>, tr!(segment_nz, recognize!(many1!(pchar))));
//* ;segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
//* pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
named!(pchar<&str, &str>, tr!(pchar, alt!(unreserved | pct_encoded | sub_delims | recognize!(one_of!(":@")))));
//* query         = *( pchar / "/" / "?" )
named!(query<&str, &str>, tr!(query, recognize!(many0!(alt!(pchar | recognize!(one_of!("/?")))))));
//* fragment      = *( pchar / "/" / "?" )
named!(fragment<&str, &str>, tr!(fragment, recognize!(many0!(alt!(pchar | recognize!(one_of!("/?")))))));
//* pct-encoded   = "%" HEXDIG HEXDIG
named!(pct_encoded<&str, &str>, tr!(pct_encoded, recognize!(do_parse!(tag!("%") >> HEXDIG >> HEXDIG >> ()))));
//* unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
named!(unreserved<&str, &str>, tr!(unreserved, alt!(ALPHA | DIGIT | recognize!(one_of!("-._~")))));
//* ;reserved      = gen-delims / sub-delims
//* ;gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
//* ;sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
//* sub-delims     =       "$" / "&" / "'" /                                     "=" / other-delims
named!(sub_delims<&str, &str>, tr!(sub_delims, alt!(recognize!(one_of!("$&'=")) | other_delims)));
//* other-delims   = "!" /                   "(" / ")" / "*" / "+" / "," / ";"
named!(other_delims<&str, &str>, tr!(other_delims, recognize!(one_of!("!()*+,;"))));
//*
//* pchar-no-SQUOTE       = unreserved / pct-encoded-no-SQUOTE / other-delims / "$" / "&" / "=" / ":" / "@"
named!(pchar_no_SQUOTE<&str, &str>, tr!(pchar_no_SQUOTE, alt!(unreserved | pct_encoded_no_SQUOTE | other_delims | recognize!(one_of!("$&=:@")))));
//* pct-encoded-no-SQUOTE = "%" ( "0" / "1" /   "3" / "4" / "5" / "6" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" / "2" / "3" / "4" / "5" / "6" /   "8" / "9" / A-to-F )
named!(pct_encoded_no_SQUOTE<&str, &str>, tr!(pct_encoded_no_SQUOTE, alt!(  recognize!(tuple!(tag!("%"), one_of!("013456789ABCDEFabcdef"), HEXDIG))
					       | recognize!(tuple!(tag!("%2"), one_of!("012345689ABCDEFabcdef")))
					      )));
//*
//* qchar-no-AMP              = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_no_AMP<&str, &str>, tr!(qchar_no_AMP, alt!(qchar_no_AMP_EQ_AT_DOLLAR | tag!("="))));
//* qchar-no-AMP-EQ           = unreserved / pct-encoded / other-delims / ":" / "@" / "/" / "?" / "$" / "'"
named!(qchar_no_AMP_EQ<&str, &str>, tr!(qchar_no_AMP_EQ, alt!(qchar_no_AMP_EQ_AT_DOLLAR | tag!("@") | tag!("$"))));
//* qchar-no-AMP-EQ-AT-DOLLAR = unreserved / pct-encoded / other-delims / ":" /       "/" / "?" /       "'"
named!(qchar_no_AMP_EQ_AT_DOLLAR<&str, &str>, tr!(qchar_no_AMP_EQ_AT_DOLLAR, alt!(unreserved | pct_encoded | other_delims | recognize!(one_of!(":/?'")))));
//*
//* qchar-unescaped       = unreserved / pct-encoded-unescaped / other-delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
named!(qchar_unescaped<&str, &str>, tr!(qchar_unescaped, alt!(unreserved | pct_encoded_unscaped | other_delims | recognize!(one_of!(":@/?$'=")))));
//* pct-encoded-unescaped = "%" ( "0" / "1" /   "3" / "4" /   "6" / "7" / "8" / "9" / A-to-F ) HEXDIG
//*                       / "%" "2" ( "0" / "1" /   "3" / "4" / "5" / "6" / "7" / "8" / "9" / A-to-F )
//*                       / "%" "5" ( DIGIT / "A" / "B" /   "D" / "E" / "F" )
named!(pct_encoded_unscaped<&str, &str>, tr!(pct_encoded_unscaped, alt!( recognize!(tuple!(tag!("%"), alt!(recognize!(one_of!("01346789")) | A_to_F), HEXDIG))
					     | recognize!(tuple!(tag!("%2"), alt!(recognize!(one_of!("013456789")) | A_to_F)))
					     | recognize!(tuple!(tag!("%5"), alt!(DIGIT | recognize!(one_of!("ABDEFabdef")))))
					     )));

//*
//* qchar-no-AMP-DQUOTE   = qchar-unescaped
//*                       / escape ( escape / quotation-mark )
named!(qchar_no_AMP_DQUOTE<&str, &str>, tr!(qchar_no_AMP_DQUOTE, alt!(qchar_unescaped | recognize!(tuple!(escape, alt!(escape | quotation_mark))))));
//*
//*
//* ;------------------------------------------------------------------------------
//* ; B. IRI syntax [RFC3987]
//* ;------------------------------------------------------------------------------
//* ; Note: these are over-generous stubs, for the actual patterns refer to RFC3987
//* ;------------------------------------------------------------------------------
//*
//* IRI-in-header = 1*( VCHAR / obs-text )
named!(IRI_in_header<&str, &str>, tr!(IRI_in_header, recognize!(many1!(alt!(VCHAR | obs_text)))));
//* IRI-in-query  = 1*qchar-no-AMP
named!(IRI_in_query<&str, &str>, tr!(IRI_in_query, recognize!(many1!(qchar_no_AMP))));

//* ;------------------------------------------------------------------------------
//* ; C. ABNF core definitions [RFC5234]
//* ;------------------------------------------------------------------------------
//*
//* ALPHA  = %x41-5A / %x61-7A
named!(ALPHA<&str, &str>, tr!(ALPHA, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_alphabetic(chr as u8)))));

//* DIGIT  = %x30-39
named!(DIGIT<&str, &str>, tr!(DIGIT, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_digit(chr as u8)))));
//
// //* HEXDIG = DIGIT / A-to-F
named!(HEXDIG<&str, &str>, tr!(HEXDIG, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && nom::is_hex_digit(chr as u8)))));

//* A-to-F = "A" / "B" / "C" / "D" / "E" / "F"
fn is_A_to_F(chr: u8) -> bool {
	(chr >= 0x41 && chr <= 0x46) || (chr >= 0x61 && chr <= 0x66)
}
named!(A_to_F<&str, &str>, tr!(A_to_F, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii() && is_A_to_F(chr as u8)))));

//* DQUOTE = %x22
named!(DQUOTE<&str, &str>, tr!(DQUOTE, tag!("\u{0022}")));

//* SP     = %x20
named!(SP<&str, &str>, tr!(SP, tag!("\u{0020}")));

//* HTAB   = %x09
named!(HTAB<&str, &str>, tr!(HTAB, tag!("\u{0009}")));

//* ;WSP    = SP / HTAB
//* ;LWSP = *(WSP / CRLF WSP)
//* VCHAR = %x21-7E
named!(VCHAR<&str, &str>, tr!(VCHAR, recognize!(verify!(nom::anychar, |chr: char| chr.is_ascii_graphic()))));

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

fn main() {
	let mut input = "resource?$filter=".to_string();

	for x in 0..200 {
		input += "id eq 1 or ";
	}
	input += "id eq 1\n";

	println!("{:?}", odataRelativeUri(&input));

	print_trace!();
	// the list of trace events can be cleared
	reset_trace!();
}
