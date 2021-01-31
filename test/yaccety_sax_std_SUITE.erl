%%----------------------------------------------------------------------
%%
%%----------------------------------------------------------------------
%% File    : yaccety_sax_std_SUITE.erl
%% Created : 2021-07-02
%%----------------------------------------------------------------------
-module(yaccety_sax_std_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("yaccety_sax.hrl").
-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: James Clark  XML 1.0 Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: not-wf-sa-001
%% Description:
%%   Attribute values must start with attribute names, not "?".
'not-wf-sa-001'(Config) -> run_test(Config, "xmltest", "not-wf/sa/001.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-002
%% Description:
%%   Names may not start with "."; it's not a Letter.
'not-wf-sa-002'(Config) -> run_test(Config, "xmltest", "not-wf/sa/002.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-003
%% Description:
%%   Processing Instruction target name is required.
'not-wf-sa-003'(Config) -> run_test(Config, "xmltest", "not-wf/sa/003.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-004
%% Description:
%%   SGML-ism: processing instructions end in '?>' not '>'.
'not-wf-sa-004'(Config) -> run_test(Config, "xmltest", "not-wf/sa/004.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-005
%% Description:
%%   Processing instructions end in '?>' not '?'.
'not-wf-sa-005'(Config) -> run_test(Config, "xmltest", "not-wf/sa/005.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-006
%% Description:
%%   XML comments may not contain "--"
'not-wf-sa-006'(Config) -> run_test(Config, "xmltest", "not-wf/sa/006.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-007
%% Description:
%%   General entity references have no whitespace after the entity name
%%   and before the semicolon.
'not-wf-sa-007'(Config) -> run_test(Config, "xmltest", "not-wf/sa/007.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-008
%% Description:
%%   Entity references must include names, which don't begin with '.'
%%   (it's not a Letter or other name start character).
'not-wf-sa-008'(Config) -> run_test(Config, "xmltest", "not-wf/sa/008.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-009
%% Description:
%%   Character references may have only decimal or numeric strings.
'not-wf-sa-009'(Config) -> run_test(Config, "xmltest", "not-wf/sa/009.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-010
%% Description:
%%   Ampersand may only appear as part of a general entity reference.
'not-wf-sa-010'(Config) -> run_test(Config, "xmltest", "not-wf/sa/010.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-011
%% Description:
%%   SGML-ism: attribute values must be explicitly assigned a value, it
%%   can't act as a boolean toggle.
'not-wf-sa-011'(Config) -> run_test(Config, "xmltest", "not-wf/sa/011.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-012
%% Description:
%%   SGML-ism: attribute values must be quoted in all cases.
'not-wf-sa-012'(Config) -> run_test(Config, "xmltest", "not-wf/sa/012.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-013
%% Description:
%%   The quotes on both ends of an attribute value must match.
'not-wf-sa-013'(Config) -> run_test(Config, "xmltest", "not-wf/sa/013.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-014
%% Description:
%%   Attribute values may not contain literal '<' characters.
'not-wf-sa-014'(Config) -> run_test(Config, "xmltest", "not-wf/sa/014.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-015
%% Description:
%%   Attribute values need a value, not just an equals sign.
'not-wf-sa-015'(Config) -> run_test(Config, "xmltest", "not-wf/sa/015.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-016
%% Description:
%%   Attribute values need an associated name.
'not-wf-sa-016'(Config) -> run_test(Config, "xmltest", "not-wf/sa/016.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-017
%% Description:
%%   CDATA sections need a terminating ']]>'.
'not-wf-sa-017'(Config) -> run_test(Config, "xmltest", "not-wf/sa/017.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-018
%% Description:
%%   CDATA sections begin with a literal '<![CDATA[', no space.
'not-wf-sa-018'(Config) -> run_test(Config, "xmltest", "not-wf/sa/018.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-019
%% Description:
%%   End tags may not be abbreviated as '</>'.
'not-wf-sa-019'(Config) -> run_test(Config, "xmltest", "not-wf/sa/019.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-020
%% Description:
%%   Attribute values may not contain literal '&' characters except as
%%   part of an entity reference.
'not-wf-sa-020'(Config) -> run_test(Config, "xmltest", "not-wf/sa/020.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-021
%% Description:
%%   Attribute values may not contain literal '&' characters except as
%%   part of an entity reference.
'not-wf-sa-021'(Config) -> run_test(Config, "xmltest", "not-wf/sa/021.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-022
%% Description:
%%   Character references end with semicolons, always!
'not-wf-sa-022'(Config) -> run_test(Config, "xmltest", "not-wf/sa/022.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-023
%% Description:
%%   Digits are not valid name start characters.
'not-wf-sa-023'(Config) -> run_test(Config, "xmltest", "not-wf/sa/023.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-024
%% Description:
%%   Digits are not valid name start characters.
'not-wf-sa-024'(Config) -> run_test(Config, "xmltest", "not-wf/sa/024.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-025
%% Description:
%%   Text may not contain a literal ']]>' sequence.
'not-wf-sa-025'(Config) -> run_test(Config, "xmltest", "not-wf/sa/025.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-026
%% Description:
%%   Text may not contain a literal ']]>' sequence.
'not-wf-sa-026'(Config) -> run_test(Config, "xmltest", "not-wf/sa/026.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-027
%% Description:
%%   Comments must be terminated with "-->".
'not-wf-sa-027'(Config) -> run_test(Config, "xmltest", "not-wf/sa/027.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-028
%% Description:
%%   Processing instructions must end with '?>'.
'not-wf-sa-028'(Config) -> run_test(Config, "xmltest", "not-wf/sa/028.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-029
%% Description:
%%   Text may not contain a literal ']]>' sequence.
'not-wf-sa-029'(Config) -> run_test(Config, "xmltest", "not-wf/sa/029.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-030
%% Description:
%%   A form feed is not a legal XML character.
'not-wf-sa-030'(Config) -> run_test(Config, "xmltest", "not-wf/sa/030.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-031
%% Description:
%%   A form feed is not a legal XML character.
'not-wf-sa-031'(Config) -> run_test(Config, "xmltest", "not-wf/sa/031.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-032
%% Description:
%%   A form feed is not a legal XML character.
'not-wf-sa-032'(Config) -> run_test(Config, "xmltest", "not-wf/sa/032.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-033
%% Description:
%%   An ESC (octal 033) is not a legal XML character.
'not-wf-sa-033'(Config) -> run_test(Config, "xmltest", "not-wf/sa/033.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-034
%% Description:
%%   A form feed is not a legal XML character.
'not-wf-sa-034'(Config) -> run_test(Config, "xmltest", "not-wf/sa/034.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-035
%% Description:
%%   The '<' character is a markup delimiter and must start an element,
%%   CDATA section, PI, or comment.
'not-wf-sa-035'(Config) -> run_test(Config, "xmltest", "not-wf/sa/035.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-036
%% Description:
%%   Text may not appear after the root element.
'not-wf-sa-036'(Config) -> run_test(Config, "xmltest", "not-wf/sa/036.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-037
%% Description:
%%   Character references may not appear after the root element.
'not-wf-sa-037'(Config) -> run_test(Config, "xmltest", "not-wf/sa/037.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-038
%% Description:
%%   Tests the "Unique Att Spec" WF constraint by providing multiple
%%   values for an attribute.
'not-wf-sa-038'(Config) -> run_test(Config, "xmltest", "not-wf/sa/038.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-039
%% Description:
%%   Tests the Element Type Match WFC - end tag name must match start tag
%%   name.
'not-wf-sa-039'(Config) -> run_test(Config, "xmltest", "not-wf/sa/039.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-040
%% Description:
%%   Provides two document elements.
'not-wf-sa-040'(Config) -> run_test(Config, "xmltest", "not-wf/sa/040.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-041
%% Description:
%%   Provides two document elements.
'not-wf-sa-041'(Config) -> run_test(Config, "xmltest", "not-wf/sa/041.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-042
%% Description:
%%   Invalid End Tag
'not-wf-sa-042'(Config) -> run_test(Config, "xmltest", "not-wf/sa/042.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-043
%% Description:
%%   Provides #PCDATA text after the document element.
'not-wf-sa-043'(Config) -> run_test(Config, "xmltest", "not-wf/sa/043.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-044
%% Description:
%%   Provides two document elements.
'not-wf-sa-044'(Config) -> run_test(Config, "xmltest", "not-wf/sa/044.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-045
%% Description:
%%   Invalid Empty Element Tag
'not-wf-sa-045'(Config) -> run_test(Config, "xmltest", "not-wf/sa/045.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-046
%% Description:
%%   This start (or empty element) tag was not terminated correctly.
'not-wf-sa-046'(Config) -> run_test(Config, "xmltest", "not-wf/sa/046.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-047
%% Description:
%%   Invalid empty element tag invalid whitespace
'not-wf-sa-047'(Config) -> run_test(Config, "xmltest", "not-wf/sa/047.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-048
%% Description:
%%   Provides a CDATA section after the root element.
'not-wf-sa-048'(Config) -> run_test(Config, "xmltest", "not-wf/sa/048.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-049
%% Description:
%%   Missing start tag
'not-wf-sa-049'(Config) -> run_test(Config, "xmltest", "not-wf/sa/049.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-050
%% Description:
%%   Empty document, with no root element.
'not-wf-sa-050'(Config) -> run_test(Config, "xmltest", "not-wf/sa/050.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-051
%% Description:
%%   CDATA is invalid at top level of document.
'not-wf-sa-051'(Config) -> run_test(Config, "xmltest", "not-wf/sa/051.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-052
%% Description:
%%   Invalid character reference.
'not-wf-sa-052'(Config) -> run_test(Config, "xmltest", "not-wf/sa/052.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-053
%% Description:
%%   End tag does not match start tag.
'not-wf-sa-053'(Config) -> run_test(Config, "xmltest", "not-wf/sa/053.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-054
%% Description:
%%   PUBLIC requires two literals.
'not-wf-sa-054'(Config) -> run_test(Config, "xmltest", "not-wf/sa/054.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-055
%% Description:
%%   Invalid Document Type Definition format.
'not-wf-sa-055'(Config) -> run_test(Config, "xmltest", "not-wf/sa/055.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-056
%% Description:
%%   Invalid Document Type Definition format - misplaced comment.
'not-wf-sa-056'(Config) -> run_test(Config, "xmltest", "not-wf/sa/056.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-057
%% Description:
%%   This isn't SGML; comments can't exist in declarations.
'not-wf-sa-057'(Config) -> run_test(Config, "xmltest", "not-wf/sa/057.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-058
%% Description:
%%   Invalid character , in ATTLIST enumeration
'not-wf-sa-058'(Config) -> run_test(Config, "xmltest", "not-wf/sa/058.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-059
%% Description:
%%   String literal must be in quotes.
'not-wf-sa-059'(Config) -> run_test(Config, "xmltest", "not-wf/sa/059.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-060
%% Description:
%%   Invalid type NAME defined in ATTLIST.
'not-wf-sa-060'(Config) -> run_test(Config, "xmltest", "not-wf/sa/060.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-061
%% Description:
%%   External entity declarations require whitespace between public and
%%   system IDs.
'not-wf-sa-061'(Config) -> run_test(Config, "xmltest", "not-wf/sa/061.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-062
%% Description:
%%   Entity declarations need space after the entity name.
'not-wf-sa-062'(Config) -> run_test(Config, "xmltest", "not-wf/sa/062.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-063
%% Description:
%%   Conditional sections may only appear in the external DTD subset.
'not-wf-sa-063'(Config) -> run_test(Config, "xmltest", "not-wf/sa/063.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-064
%% Description:
%%   Space is required between attribute type and default values in
%%   <!ATTLIST...> declarations.
'not-wf-sa-064'(Config) -> run_test(Config, "xmltest", "not-wf/sa/064.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-065
%% Description:
%%   Space is required between attribute name and type in <!ATTLIST...>
%%   declarations.
'not-wf-sa-065'(Config) -> run_test(Config, "xmltest", "not-wf/sa/065.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-066
%% Description:
%%   Required whitespace is missing.
'not-wf-sa-066'(Config) -> run_test(Config, "xmltest", "not-wf/sa/066.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-067
%% Description:
%%   Space is required between attribute type and default values in
%%   <!ATTLIST...> declarations.
'not-wf-sa-067'(Config) -> run_test(Config, "xmltest", "not-wf/sa/067.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-068
%% Description:
%%   Space is required between NOTATION keyword and list of enumerated
%%   choices in <!ATTLIST...> declarations.
'not-wf-sa-068'(Config) -> run_test(Config, "xmltest", "not-wf/sa/068.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-069
%% Description:
%%   Space is required before an NDATA entity annotation.
'not-wf-sa-069'(Config) -> run_test(Config, "xmltest", "not-wf/sa/069.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-070
%% Description:
%%   XML comments may not contain "--"
'not-wf-sa-070'(Config) -> run_test(Config, "xmltest", "not-wf/sa/070.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-071
%% Description:
%%   ENTITY can't reference itself directly or indirectly.
'not-wf-sa-071'(Config) -> run_test(Config, "xmltest", "not-wf/sa/071.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-072
%% Description:
%%   Undefined ENTITY foo.
'not-wf-sa-072'(Config) -> run_test(Config, "xmltest", "not-wf/sa/072.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-073
%% Description:
%%   Undefined ENTITY f.
'not-wf-sa-073'(Config) -> run_test(Config, "xmltest", "not-wf/sa/073.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-074
%% Description:
%%   Internal general parsed entities are only well formed if they match
%%   the "content" production.
'not-wf-sa-074'(Config) -> run_test(Config, "xmltest", "not-wf/sa/074.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-075
%% Description:
%%   ENTITY can't reference itself directly or indirectly.
'not-wf-sa-075'(Config) -> run_test(Config, "xmltest", "not-wf/sa/075.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-076
%% Description:
%%   Undefined ENTITY foo.
'not-wf-sa-076'(Config) -> run_test(Config, "xmltest", "not-wf/sa/076.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-077
%% Description:
%%   Undefined ENTITY bar.
'not-wf-sa-077'(Config) -> run_test(Config, "xmltest", "not-wf/sa/077.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-078
%% Description:
%%   Undefined ENTITY foo.
'not-wf-sa-078'(Config) -> run_test(Config, "xmltest", "not-wf/sa/078.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-079
%% Description:
%%   ENTITY can't reference itself directly or indirectly.
'not-wf-sa-079'(Config) -> run_test(Config, "xmltest", "not-wf/sa/079.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-080
%% Description:
%%   ENTITY can't reference itself directly or indirectly.
'not-wf-sa-080'(Config) -> run_test(Config, "xmltest", "not-wf/sa/080.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-081
%% Entities: general
%% Description:
%%   This tests the WFC, since the entity is referred to within an
%%   attribute.
'not-wf-sa-081'(Config) -> run_test(Config, "xmltest", "not-wf/sa/081.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-082
%% Entities: general
%% Description:
%%   This tests the WFC, since the entity is referred to within an
%%   attribute.
'not-wf-sa-082'(Config) -> run_test(Config, "xmltest", "not-wf/sa/082.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-083
%% Description:
%%   Undefined NOTATION n.
'not-wf-sa-083'(Config) -> run_test(Config, "xmltest", "not-wf/sa/083.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-084
%% Description:
%%   Tests the WFC by referring to an unparsed entity. (This precedes the
%%   error of not declaring that entity's notation, which may be detected
%%   any time before the DTD parsing is completed.)
'not-wf-sa-084'(Config) -> run_test(Config, "xmltest", "not-wf/sa/084.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-085
%% Description:
%%   Public IDs may not contain "[".
'not-wf-sa-085'(Config) -> run_test(Config, "xmltest", "not-wf/sa/085.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-086
%% Description:
%%   Public IDs may not contain "[".
'not-wf-sa-086'(Config) -> run_test(Config, "xmltest", "not-wf/sa/086.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-087
%% Description:
%%   Public IDs may not contain "[".
'not-wf-sa-087'(Config) -> run_test(Config, "xmltest", "not-wf/sa/087.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-088
%% Description:
%%   Attribute values are terminated by literal quote characters, and any
%%   entity expansion is done afterwards.
'not-wf-sa-088'(Config) -> run_test(Config, "xmltest", "not-wf/sa/088.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-089
%% Description:
%%   Parameter entities "are" always parsed; NDATA annotations are not
%%   permitted.
'not-wf-sa-089'(Config) -> run_test(Config, "xmltest", "not-wf/sa/089.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-090
%% Description:
%%   Attributes may not contain a literal "<" character; this one has one
%%   because of reference expansion.
'not-wf-sa-090'(Config) -> run_test(Config, "xmltest", "not-wf/sa/090.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-091
%% Description:
%%   Parameter entities "are" always parsed; NDATA annotations are not
%%   permitted.
'not-wf-sa-091'(Config) -> run_test(Config, "xmltest", "not-wf/sa/091.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-092
%% Description:
%%   The replacement text of this entity has an illegal reference,
%%   because the character reference is expanded immediately.
'not-wf-sa-092'(Config) -> run_test(Config, "xmltest", "not-wf/sa/092.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-093
%% Description:
%%   Hexadecimal character references may not use the uppercase 'X'.
'not-wf-sa-093'(Config) -> run_test(Config, "xmltest", "not-wf/sa/093.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-094
%% Description:
%%   Prolog VERSION must be lowercase.
'not-wf-sa-094'(Config) -> run_test(Config, "xmltest", "not-wf/sa/094.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-095
%% Description:
%%   VersionInfo must come before EncodingDecl.
'not-wf-sa-095'(Config) -> run_test(Config, "xmltest", "not-wf/sa/095.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-096
%% Description:
%%   Space is required before the standalone declaration.
'not-wf-sa-096'(Config) -> run_test(Config, "xmltest", "not-wf/sa/096.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-097
%% Description:
%%   Both quotes surrounding VersionNum must be the same.
'not-wf-sa-097'(Config) -> run_test(Config, "xmltest", "not-wf/sa/097.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-098
%% Description:
%%   Only one "version=..." string may appear in an XML declaration.
'not-wf-sa-098'(Config) -> run_test(Config, "xmltest", "not-wf/sa/098.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-099
%% Description:
%%   Only three pseudo-attributes are in the XML declaration, and
%%   "valid=..." is not one of them.
'not-wf-sa-099'(Config) -> run_test(Config, "xmltest", "not-wf/sa/099.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-100
%% Description:
%%   Only "yes" and "no" are permitted as values of "standalone".
'not-wf-sa-100'(Config) -> run_test(Config, "xmltest", "not-wf/sa/100.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-101
%% Description:
%%   Space is not permitted in an encoding name.
'not-wf-sa-101'(Config) -> run_test(Config, "xmltest", "not-wf/sa/101.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-102
%% Description:
%%   Provides an illegal XML version number; spaces are illegal.
'not-wf-sa-102'(Config) -> run_test(Config, "xmltest", "not-wf/sa/102.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-103
%% Description:
%%   End-tag required for element foo.
'not-wf-sa-103'(Config) -> run_test(Config, "xmltest", "not-wf/sa/103.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-104
%% Description:
%%   Internal general parsed entities are only well formed if they match
%%   the "content" production.
'not-wf-sa-104'(Config) -> run_test(Config, "xmltest", "not-wf/sa/104.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-105
%% Description:
%%   Invalid placement of CDATA section.
'not-wf-sa-105'(Config) -> run_test(Config, "xmltest", "not-wf/sa/105.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-106
%% Description:
%%   Invalid placement of entity declaration.
'not-wf-sa-106'(Config) -> run_test(Config, "xmltest", "not-wf/sa/106.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-107
%% Description:
%%   Invalid document type declaration. CDATA alone is invalid.
'not-wf-sa-107'(Config) -> run_test(Config, "xmltest", "not-wf/sa/107.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-108
%% Description:
%%   No space in '<![CDATA['.
'not-wf-sa-108'(Config) -> run_test(Config, "xmltest", "not-wf/sa/108.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-109
%% Description:
%%   Tags invalid within EntityDecl.
'not-wf-sa-109'(Config) -> run_test(Config, "xmltest", "not-wf/sa/109.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-110
%% Description:
%%   Entity reference must be in content of element.
'not-wf-sa-110'(Config) -> run_test(Config, "xmltest", "not-wf/sa/110.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-111
%% Description:
%%   Entiry reference must be in content of element not Start-tag.
'not-wf-sa-111'(Config) -> run_test(Config, "xmltest", "not-wf/sa/111.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-112
%% Description:
%%   CDATA sections start '<![CDATA[', not '<!cdata['.
'not-wf-sa-112'(Config) -> run_test(Config, "xmltest", "not-wf/sa/112.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-113
%% Description:
%%   Parameter entity values must use valid reference syntax; this
%%   reference is malformed.
'not-wf-sa-113'(Config) -> run_test(Config, "xmltest", "not-wf/sa/113.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-114
%% Description:
%%   General entity values must use valid reference syntax; this
%%   reference is malformed.
'not-wf-sa-114'(Config) -> run_test(Config, "xmltest", "not-wf/sa/114.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-115
%% Description:
%%   The replacement text of this entity is an illegal character
%%   reference, which must be rejected when it is parsed in the context
%%   of an attribute value.
'not-wf-sa-115'(Config) -> run_test(Config, "xmltest", "not-wf/sa/115.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-116
%% Description:
%%   Internal general parsed entities are only well formed if they match
%%   the "content" production. This is a partial character reference, not
%%   a full one.
'not-wf-sa-116'(Config) -> run_test(Config, "xmltest", "not-wf/sa/116.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-117
%% Description:
%%   Internal general parsed entities are only well formed if they match
%%   the "content" production. This is a partial character reference, not
%%   a full one.
'not-wf-sa-117'(Config) -> run_test(Config, "xmltest", "not-wf/sa/117.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-118
%% Description:
%%   Entity reference expansion is not recursive.
'not-wf-sa-118'(Config) -> run_test(Config, "xmltest", "not-wf/sa/118.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-119
%% Description:
%%   Internal general parsed entities are only well formed if they match
%%   the "content" production. This is a partial character reference, not
%%   a full one.
'not-wf-sa-119'(Config) -> run_test(Config, "xmltest", "not-wf/sa/119.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-120
%% Description:
%%   Character references are expanded in the replacement text of an
%%   internal entity, which is then parsed as usual. Accordingly, & must
%%   be doubly quoted - encoded either as or as .
'not-wf-sa-120'(Config) -> run_test(Config, "xmltest", "not-wf/sa/120.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-121
%% Description:
%%   A name of an ENTITY was started with an invalid character.
'not-wf-sa-121'(Config) -> run_test(Config, "xmltest", "not-wf/sa/121.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-122
%% Description:
%%   Invalid syntax mixed connectors are used.
'not-wf-sa-122'(Config) -> run_test(Config, "xmltest", "not-wf/sa/122.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-123
%% Description:
%%   Invalid syntax mismatched parenthesis.
'not-wf-sa-123'(Config) -> run_test(Config, "xmltest", "not-wf/sa/123.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-124
%% Description:
%%   Invalid format of Mixed-content declaration.
'not-wf-sa-124'(Config) -> run_test(Config, "xmltest", "not-wf/sa/124.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-125
%% Description:
%%   Invalid syntax extra set of parenthesis not necessary.
'not-wf-sa-125'(Config) -> run_test(Config, "xmltest", "not-wf/sa/125.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-126
%% Description:
%%   Invalid syntax Mixed-content must be defined as zero or more.
'not-wf-sa-126'(Config) -> run_test(Config, "xmltest", "not-wf/sa/126.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-127
%% Description:
%%   Invalid syntax Mixed-content must be defined as zero or more.
'not-wf-sa-127'(Config) -> run_test(Config, "xmltest", "not-wf/sa/127.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-128
%% Description:
%%   Invalid CDATA syntax.
'not-wf-sa-128'(Config) -> run_test(Config, "xmltest", "not-wf/sa/128.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-129
%% Description:
%%   Invalid syntax for Element Type Declaration.
'not-wf-sa-129'(Config) -> run_test(Config, "xmltest", "not-wf/sa/129.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-130
%% Description:
%%   Invalid syntax for Element Type Declaration.
'not-wf-sa-130'(Config) -> run_test(Config, "xmltest", "not-wf/sa/130.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-131
%% Description:
%%   Invalid syntax for Element Type Declaration.
'not-wf-sa-131'(Config) -> run_test(Config, "xmltest", "not-wf/sa/131.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-132
%% Description:
%%   Invalid syntax mixed connectors used.
'not-wf-sa-132'(Config) -> run_test(Config, "xmltest", "not-wf/sa/132.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-133
%% Description:
%%   Illegal whitespace before optional character causes syntax error.
'not-wf-sa-133'(Config) -> run_test(Config, "xmltest", "not-wf/sa/133.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-134
%% Description:
%%   Illegal whitespace before optional character causes syntax error.
'not-wf-sa-134'(Config) -> run_test(Config, "xmltest", "not-wf/sa/134.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-135
%% Description:
%%   Invalid character used as connector.
'not-wf-sa-135'(Config) -> run_test(Config, "xmltest", "not-wf/sa/135.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-136
%% Description:
%%   Tag omission is invalid in XML.
'not-wf-sa-136'(Config) -> run_test(Config, "xmltest", "not-wf/sa/136.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-137
%% Description:
%%   Space is required before a content model.
'not-wf-sa-137'(Config) -> run_test(Config, "xmltest", "not-wf/sa/137.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-138
%% Description:
%%   Invalid syntax for content particle.
'not-wf-sa-138'(Config) -> run_test(Config, "xmltest", "not-wf/sa/138.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-139
%% Description:
%%   The element-content model should not be empty.
'not-wf-sa-139'(Config) -> run_test(Config, "xmltest", "not-wf/sa/139.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-142
%% Description:
%%   Character #x0000 is not legal anywhere in an XML document.
'not-wf-sa-142'(Config) -> run_test(Config, "xmltest", "not-wf/sa/142.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-143
%% Description:
%%   Character #x001F is not legal anywhere in an XML document.
'not-wf-sa-143'(Config) -> run_test(Config, "xmltest", "not-wf/sa/143.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-144
%% Description:
%%   Character #xFFFF is not legal anywhere in an XML document.
'not-wf-sa-144'(Config) -> run_test(Config, "xmltest", "not-wf/sa/144.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-145
%% Description:
%%   Character #xD800 is not legal anywhere in an XML document. (If it
%%   appeared in a UTF-16 surrogate pair, it'd represent half of a UCS-4
%%   character and so wouldn't really be in the document.)
'not-wf-sa-145'(Config) -> run_test(Config, "xmltest", "not-wf/sa/145.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-146
%% Description:
%%   Character references must also refer to legal XML characters;
%%   #x00110000 is one more than the largest legal character.
'not-wf-sa-146'(Config) -> run_test(Config, "xmltest", "not-wf/sa/146.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-147
%% Description:
%%   XML Declaration may not be preceded by whitespace.
'not-wf-sa-147'(Config) -> run_test(Config, "xmltest", "not-wf/sa/147.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-148
%% Description:
%%   XML Declaration may not be preceded by comments or whitespace.
'not-wf-sa-148'(Config) -> run_test(Config, "xmltest", "not-wf/sa/148.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-149
%% Description:
%%   XML Declaration may not be within a DTD.
'not-wf-sa-149'(Config) -> run_test(Config, "xmltest", "not-wf/sa/149.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-150
%% Description:
%%   XML declarations may not be within element content.
'not-wf-sa-150'(Config) -> run_test(Config, "xmltest", "not-wf/sa/150.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-151
%% Description:
%%   XML declarations may not follow document content.
'not-wf-sa-151'(Config) -> run_test(Config, "xmltest", "not-wf/sa/151.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-152
%% Description:
%%   XML declarations must include the "version=..." string.
'not-wf-sa-152'(Config) -> run_test(Config, "xmltest", "not-wf/sa/152.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-153
%% Description:
%%   Text declarations may not begin internal parsed entities; they may
%%   only appear at the beginning of external parsed (parameter or
%%   general) entities.
'not-wf-sa-153'(Config) -> run_test(Config, "xmltest", "not-wf/sa/153.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-154
%% Description:
%%   '<?XML ...?>' is neither an XML declaration nor a legal processing
%%   instruction target name.
'not-wf-sa-154'(Config) -> run_test(Config, "xmltest", "not-wf/sa/154.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-155
%% Description:
%%   '<?xmL ...?>' is neither an XML declaration nor a legal processing
%%   instruction target name.
'not-wf-sa-155'(Config) -> run_test(Config, "xmltest", "not-wf/sa/155.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-156
%% Description:
%%   '<?xMl ...?>' is neither an XML declaration nor a legal processing
%%   instruction target name.
'not-wf-sa-156'(Config) -> run_test(Config, "xmltest", "not-wf/sa/156.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-157
%% Description:
%%   '<?xmL ...?>' is not a legal processing instruction target name.
'not-wf-sa-157'(Config) -> run_test(Config, "xmltest", "not-wf/sa/157.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-158
%% Description:
%%   SGML-ism: "#NOTATION gif" can't have attributes.
'not-wf-sa-158'(Config) -> run_test(Config, "xmltest", "not-wf/sa/158.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-159
%% Description:
%%   Uses '&' unquoted in an entity declaration, which is illegal syntax
%%   for an entity reference.
'not-wf-sa-159'(Config) -> run_test(Config, "xmltest", "not-wf/sa/159.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-160
%% Description:
%%   Violates the WFC by using a PE reference within a declaration.
'not-wf-sa-160'(Config) -> run_test(Config, "xmltest", "not-wf/sa/160.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-161
%% Description:
%%   Violates the WFC by using a PE reference within a declaration.
'not-wf-sa-161'(Config) -> run_test(Config, "xmltest", "not-wf/sa/161.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-162
%% Description:
%%   Violates the WFC by using a PE reference within a declaration.
'not-wf-sa-162'(Config) -> run_test(Config, "xmltest", "not-wf/sa/162.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-163
%% Description:
%%   Invalid placement of Parameter entity reference.
'not-wf-sa-163'(Config) -> run_test(Config, "xmltest", "not-wf/sa/163.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-164
%% Description:
%%   Invalid placement of Parameter entity reference.
'not-wf-sa-164'(Config) -> run_test(Config, "xmltest", "not-wf/sa/164.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-165
%% Description:
%%   Parameter entity declarations must have a space before the '%'.
'not-wf-sa-165'(Config) -> run_test(Config, "xmltest", "not-wf/sa/165.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-166
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-166'(Config) -> run_test(Config, "xmltest", "not-wf/sa/166.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-167
%% Description:
%%   Character FFFE is not legal anywhere in an XML document.
'not-wf-sa-167'(Config) -> run_test(Config, "xmltest", "not-wf/sa/167.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-168
%% Description:
%%   An unpaired surrogate (D800) is not legal anywhere in an XML
%%   document.
'not-wf-sa-168'(Config) -> run_test(Config, "xmltest", "not-wf/sa/168.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-169
%% Description:
%%   An unpaired surrogate (DC00) is not legal anywhere in an XML
%%   document.
'not-wf-sa-169'(Config) -> run_test(Config, "xmltest", "not-wf/sa/169.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-170
%% Description:
%%   Four byte UTF-8 encodings can encode UCS-4 characters which are
%%   beyond the range of legal XML characters (and can't be expressed in
%%   Unicode surrogate pairs). This document holds such a character.
'not-wf-sa-170'(Config) -> run_test(Config, "xmltest", "not-wf/sa/170.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-171
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-171'(Config) -> run_test(Config, "xmltest", "not-wf/sa/171.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-172
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-172'(Config) -> run_test(Config, "xmltest", "not-wf/sa/172.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-173
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-173'(Config) -> run_test(Config, "xmltest", "not-wf/sa/173.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-174
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-174'(Config) -> run_test(Config, "xmltest", "not-wf/sa/174.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-175
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-175'(Config) -> run_test(Config, "xmltest", "not-wf/sa/175.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-176
%% Description:
%%   Start tags must have matching end tags.
'not-wf-sa-176'(Config) -> run_test(Config, "xmltest", "not-wf/sa/176.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-177
%% Description:
%%   Character FFFF is not legal anywhere in an XML document.
'not-wf-sa-177'(Config) -> run_test(Config, "xmltest", "not-wf/sa/177.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-178
%% Description:
%%   Invalid syntax matching double quote is missing.
'not-wf-sa-178'(Config) -> run_test(Config, "xmltest", "not-wf/sa/178.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-179
%% Description:
%%   Invalid syntax matching double quote is missing.
'not-wf-sa-179'(Config) -> run_test(Config, "xmltest", "not-wf/sa/179.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-180
%% Description:
%%   The WFC requires entities to be declared before they are used in an
%%   attribute list declaration.
'not-wf-sa-180'(Config) -> run_test(Config, "xmltest", "not-wf/sa/180.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-181
%% Description:
%%   Internal parsed entities must match the production to be well
%%   formed.
'not-wf-sa-181'(Config) -> run_test(Config, "xmltest", "not-wf/sa/181.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-182
%% Description:
%%   Internal parsed entities must match the production to be well
%%   formed.
'not-wf-sa-182'(Config) -> run_test(Config, "xmltest", "not-wf/sa/182.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-183
%% Description:
%%   Mixed content declarations may not include content particles.
'not-wf-sa-183'(Config) -> run_test(Config, "xmltest", "not-wf/sa/183.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-184
%% Description:
%%   In mixed content models, element names must not be parenthesized.
'not-wf-sa-184'(Config) -> run_test(Config, "xmltest", "not-wf/sa/184.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-185
%% Entities: parameter
%% Description:
%%   Tests the WFC. a nonvalidating parser is permitted not to report
%%   this WFC violation, since it would need to read an external
%%   parameter entity to distinguish it from a violation of the VC.
'not-wf-sa-185'(Config) -> run_test(Config, "xmltest", "not-wf/sa/185.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-sa-186
%% Description:
%%   Whitespace is required between attribute/value pairs.
'not-wf-sa-186'(Config) -> run_test(Config, "xmltest", "not-wf/sa/186.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-001
%% Entities: both
%% Description:
%%   Conditional sections must be properly terminated ("]>" used instead
%%   of "]]>").
'not-wf-not-sa-001'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/001.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-002
%% Entities: both
%% Description:
%%   Processing instruction target names may not be "XML" in any
%%   combination of cases.
'not-wf-not-sa-002'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/002.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-003
%% Entities: both
%% Description:
%%   Conditional sections must be properly terminated ("]]>" omitted).
'not-wf-not-sa-003'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/003.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-004
%% Entities: both
%% Description:
%%   Conditional sections must be properly terminated ("]]>" omitted).
'not-wf-not-sa-004'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/004.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-005
%% Entities: both
%% Description:
%%   Tests the VC by referring to an undefined parameter entity within an
%%   external entity.

%% run_test(Config, "xmltest", "not-wf/not-sa/005.xml", "error").
'not-wf-not-sa-005'(_Config) -> {skip, "unknown parameter reference in external (VC test not WFC)"}.

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-006
%% Entities: both
%% Description:
%%   Conditional sections need a '[' after the INCLUDE or IGNORE.
'not-wf-not-sa-006'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/006.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-007
%% Entities: both
%% Description:
%%   A <!DOCTYPE ...> declaration may not begin any external entity; it's
%%   only found once, in the document entity.
'not-wf-not-sa-007'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/007.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-008
%% Entities: both
%% Description:
%%   In DTDs, the '%' character must be part of a parameter entity
%%   reference.
'not-wf-not-sa-008'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/008.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-not-sa-009
%% Entities: both
%% Description:
%%   This test violates WFC:PE Between Declarations in Production 28a.
%%   The last character of a markup declaration is not contained in the
%%   same parameter-entity text replacement.
'not-wf-not-sa-009'(Config) -> run_test(Config, "xmltest", "not-wf/not-sa/009.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-ext-sa-001
%% Entities: both
%% Description:
%%   Tests the WFC by having an external general entity be
%%   self-recursive.

%% run_test(Config, "xmltest", "not-wf/ext-sa/001.xml", "not-wf").
'not-wf-ext-sa-001'(_Config) -> {skip, "recursive external reference"}.

%%----------------------------------------------------------------------
%% ID: not-wf-ext-sa-002
%% Entities: both
%% Description:
%%   External entities have "text declarations", which do not permit the
%%   "standalone=..." attribute that's allowed in XML declarations.
'not-wf-ext-sa-002'(Config) -> run_test(Config, "xmltest", "not-wf/ext-sa/002.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: not-wf-ext-sa-003
%% Entities: both
%% Description:
%%   Only one text declaration is permitted; a second one looks like an
%%   illegal processing instruction (target names of "xml" in any case
%%   are not allowed).
'not-wf-ext-sa-003'(Config) -> run_test(Config, "xmltest", "not-wf/ext-sa/003.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: invalid--002
%% Entities: both
%% Description:
%%   Tests the "Proper Group/PE Nesting" validity constraint by
%%   fragmenting a content model between two parameter entities.
'invalid--002'(Config) -> run_test(Config, "xmltest", "invalid/002.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: invalid--005
%% Entities: both
%% Description:
%%   Tests the "Proper Declaration/PE Nesting" validity constraint by
%%   fragmenting an element declaration between two parameter entities.
'invalid--005'(Config) -> run_test(Config, "xmltest", "invalid/005.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: invalid--006
%% Entities: both
%% Description:
%%   Tests the "Proper Declaration/PE Nesting" validity constraint by
%%   fragmenting an element declaration between two parameter entities.
'invalid--006'(Config) -> run_test(Config, "xmltest", "invalid/006.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: invalid-not-sa-022
%% Entities: both
%% Output: invalid/not-sa/out/022.xml
%% Description:
%%   Test the "Proper Conditional Section/ PE Nesting" validity
%%   constraint.
'invalid-not-sa-022'(Config) ->
    run_test(Config, "xmltest", "invalid/not-sa/022.xml", "invalid", "invalid/not-sa/out/022.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-001
%% Output: valid/sa/out/001.xml
%% Description:
%%   Test demonstrates an Element Type Declaration with Mixed Content.
'valid-sa-001'(Config) ->
    run_test(Config, "xmltest", "valid/sa/001.xml", "valid", "valid/sa/out/001.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-002
%% Output: valid/sa/out/002.xml
%% Description:
%%   Test demonstrates that whitespace is permitted after the tag name in
%%   a Start-tag.
'valid-sa-002'(Config) ->
    run_test(Config, "xmltest", "valid/sa/002.xml", "valid", "valid/sa/out/002.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-003
%% Output: valid/sa/out/003.xml
%% Description:
%%   Test demonstrates that whitespace is permitted after the tag name in
%%   an End-tag.
'valid-sa-003'(Config) ->
    run_test(Config, "xmltest", "valid/sa/003.xml", "valid", "valid/sa/out/003.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-004
%% Output: valid/sa/out/004.xml
%% Description:
%%   Test demonstrates a valid attribute specification within a
%%   Start-tag.
'valid-sa-004'(Config) ->
    run_test(Config, "xmltest", "valid/sa/004.xml", "valid", "valid/sa/out/004.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-005
%% Output: valid/sa/out/005.xml
%% Description:
%%   Test demonstrates a valid attribute specification within a Start-tag
%%   that contains whitespace on both sides of the equal sign.
'valid-sa-005'(Config) ->
    run_test(Config, "xmltest", "valid/sa/005.xml", "valid", "valid/sa/out/005.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-006
%% Output: valid/sa/out/006.xml
%% Description:
%%   Test demonstrates that the AttValue within a Start-tag can use a
%%   single quote as a delimter.
'valid-sa-006'(Config) ->
    run_test(Config, "xmltest", "valid/sa/006.xml", "valid", "valid/sa/out/006.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-007
%% Output: valid/sa/out/007.xml
%% Description:
%%   Test demonstrates numeric character references can be used for
%%   element content.
'valid-sa-007'(Config) ->
    run_test(Config, "xmltest", "valid/sa/007.xml", "valid", "valid/sa/out/007.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-008
%% Output: valid/sa/out/008.xml
%% Description:
%%   Test demonstrates character references can be used for element
%%   content.
'valid-sa-008'(Config) ->
    run_test(Config, "xmltest", "valid/sa/008.xml", "valid", "valid/sa/out/008.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-009
%% Output: valid/sa/out/009.xml
%% Description:
%%   Test demonstrates that PubidChar can be used for element content.
'valid-sa-009'(Config) ->
    run_test(Config, "xmltest", "valid/sa/009.xml", "valid", "valid/sa/out/009.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-010
%% Output: valid/sa/out/010.xml
%% Description:
%%   Test demonstrates that whitespace is valid after the Attribute in a
%%   Start-tag.
'valid-sa-010'(Config) ->
    run_test(Config, "xmltest", "valid/sa/010.xml", "valid", "valid/sa/out/010.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-011
%% Output: valid/sa/out/011.xml
%% Description:
%%   Test demonstrates mutliple Attibutes within the Start-tag.
'valid-sa-011'(Config) ->
    run_test(Config, "xmltest", "valid/sa/011.xml", "valid", "valid/sa/out/011.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-012
%% Output: valid/sa/out/012.xml
%% Description:
%%   Uses a legal XML 1.0 name consisting of a single colon character
%%   (disallowed by the latest XML Namespaces draft).

%% run_test(Config, "xmltest", "valid/sa/012.xml", "valid", "valid/sa/out/012.xml").
'valid-sa-012'(_Config) ->
    {skip,
        "Uses a legal XML 1.0 name consisting of a single colon character (disallowed by the latest XML Namespaces draft)."}.

%%----------------------------------------------------------------------
%% ID: valid-sa-013
%% Output: valid/sa/out/013.xml
%% Description:
%%   Test demonstrates that the Attribute in a Start-tag can consist of
%%   numerals along with special characters.
'valid-sa-013'(Config) ->
    run_test(Config, "xmltest", "valid/sa/013.xml", "valid", "valid/sa/out/013.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-014
%% Output: valid/sa/out/014.xml
%% Description:
%%   Test demonstrates that all lower case letters are valid for the
%%   Attribute in a Start-tag.
'valid-sa-014'(Config) ->
    run_test(Config, "xmltest", "valid/sa/014.xml", "valid", "valid/sa/out/014.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-015
%% Output: valid/sa/out/015.xml
%% Description:
%%   Test demonstrates that all upper case letters are valid for the
%%   Attribute in a Start-tag.
'valid-sa-015'(Config) ->
    run_test(Config, "xmltest", "valid/sa/015.xml", "valid", "valid/sa/out/015.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-016
%% Output: valid/sa/out/016.xml
%% Description:
%%   Test demonstrates that Processing Instructions are valid element
%%   content.
'valid-sa-016'(Config) ->
    run_test(Config, "xmltest", "valid/sa/016.xml", "valid", "valid/sa/out/016.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-017
%% Output: valid/sa/out/017.xml
%% Description:
%%   Test demonstrates that Processing Instructions are valid element
%%   content and there can be more than one.
'valid-sa-017'(Config) ->
    run_test(Config, "xmltest", "valid/sa/017.xml", "valid", "valid/sa/out/017.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-018
%% Output: valid/sa/out/018.xml
%% Description:
%%   Test demonstrates that CDATA sections are valid element content.
'valid-sa-018'(Config) ->
    run_test(Config, "xmltest", "valid/sa/018.xml", "valid", "valid/sa/out/018.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-019
%% Output: valid/sa/out/019.xml
%% Description:
%%   Test demonstrates that CDATA sections are valid element content and
%%   that ampersands may occur in their literal form.
'valid-sa-019'(Config) ->
    run_test(Config, "xmltest", "valid/sa/019.xml", "valid", "valid/sa/out/019.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-020
%% Output: valid/sa/out/020.xml
%% Description:
%%   Test demonstractes that CDATA sections are valid element content and
%%   that everyting between the CDStart and CDEnd is recognized as
%%   character data not markup.
'valid-sa-020'(Config) ->
    run_test(Config, "xmltest", "valid/sa/020.xml", "valid", "valid/sa/out/020.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-021
%% Output: valid/sa/out/021.xml
%% Description:
%%   Test demonstrates that comments are valid element content.
'valid-sa-021'(Config) ->
    run_test(Config, "xmltest", "valid/sa/021.xml", "valid", "valid/sa/out/021.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-022
%% Output: valid/sa/out/022.xml
%% Description:
%%   Test demonstrates that comments are valid element content and that
%%   all characters before the double-hypen right angle combination are
%%   considered part of thecomment.
'valid-sa-022'(Config) ->
    run_test(Config, "xmltest", "valid/sa/022.xml", "valid", "valid/sa/out/022.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-023
%% Output: valid/sa/out/023.xml
%% Description:
%%   Test demonstrates that Entity References are valid element content.
'valid-sa-023'(Config) ->
    run_test(Config, "xmltest", "valid/sa/023.xml", "valid", "valid/sa/out/023.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-024
%% Output: valid/sa/out/024.xml
%% Description:
%%   Test demonstrates that Entity References are valid element content
%%   and also demonstrates a valid Entity Declaration.
'valid-sa-024'(Config) ->
    run_test(Config, "xmltest", "valid/sa/024.xml", "valid", "valid/sa/out/024.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-025
%% Output: valid/sa/out/025.xml
%% Description:
%%   Test demonstrates an Element Type Declaration and that the
%%   contentspec can be of mixed content.
'valid-sa-025'(Config) ->
    run_test(Config, "xmltest", "valid/sa/025.xml", "valid", "valid/sa/out/025.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-026
%% Output: valid/sa/out/026.xml
%% Description:
%%   Test demonstrates an Element Type Declaration and that EMPTY is a
%%   valid contentspec.
'valid-sa-026'(Config) ->
    run_test(Config, "xmltest", "valid/sa/026.xml", "valid", "valid/sa/out/026.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-027
%% Output: valid/sa/out/027.xml
%% Description:
%%   Test demonstrates an Element Type Declaration and that ANY is a
%%   valid contenspec.
'valid-sa-027'(Config) ->
    run_test(Config, "xmltest", "valid/sa/027.xml", "valid", "valid/sa/out/027.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-028
%% Output: valid/sa/out/028.xml
%% Description:
%%   Test demonstrates a valid prolog that uses double quotes as
%%   delimeters around the VersionNum.
'valid-sa-028'(Config) ->
    run_test(Config, "xmltest", "valid/sa/028.xml", "valid", "valid/sa/out/028.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-029
%% Output: valid/sa/out/029.xml
%% Description:
%%   Test demonstrates a valid prolog that uses single quotes as
%%   delimters around the VersionNum.
'valid-sa-029'(Config) ->
    run_test(Config, "xmltest", "valid/sa/029.xml", "valid", "valid/sa/out/029.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-030
%% Output: valid/sa/out/030.xml
%% Description:
%%   Test demonstrates a valid prolog that contains whitespace on both
%%   sides of the equal sign in the VersionInfo.
'valid-sa-030'(Config) ->
    run_test(Config, "xmltest", "valid/sa/030.xml", "valid", "valid/sa/out/030.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-031
%% Output: valid/sa/out/031.xml
%% Description:
%%   Test demonstrates a valid EncodingDecl within the prolog.
'valid-sa-031'(Config) ->
    run_test(Config, "xmltest", "valid/sa/031.xml", "valid", "valid/sa/out/031.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-032
%% Output: valid/sa/out/032.xml
%% Description:
%%   Test demonstrates a valid SDDecl within the prolog.
'valid-sa-032'(Config) ->
    run_test(Config, "xmltest", "valid/sa/032.xml", "valid", "valid/sa/out/032.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-033
%% Output: valid/sa/out/033.xml
%% Description:
%%   Test demonstrates that both a EncodingDecl and SDDecl are valid
%%   within the prolog.
'valid-sa-033'(Config) ->
    run_test(Config, "xmltest", "valid/sa/033.xml", "valid", "valid/sa/out/033.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-034
%% Output: valid/sa/out/034.xml
%% Description:
%%   Test demonstrates the correct syntax for an Empty element tag.
'valid-sa-034'(Config) ->
    run_test(Config, "xmltest", "valid/sa/034.xml", "valid", "valid/sa/out/034.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-035
%% Output: valid/sa/out/035.xml
%% Description:
%%   Test demonstrates that whitespace is permissible after the name in
%%   an Empty element tag.
'valid-sa-035'(Config) ->
    run_test(Config, "xmltest", "valid/sa/035.xml", "valid", "valid/sa/out/035.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-036
%% Output: valid/sa/out/036.xml
%% Description:
%%   Test demonstrates a valid processing instruction.
'valid-sa-036'(Config) ->
    run_test(Config, "xmltest", "valid/sa/036.xml", "valid", "valid/sa/out/036.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-017a
%% Output: valid/sa/out/017a.xml
%% Description:
%%   Test demonstrates that two apparently wrong Processing Instructions
%%   make a right one, with very odd content "some data ? > <?".
'valid-sa-017a'(Config) ->
    run_test(Config, "xmltest", "valid/sa/017a.xml", "valid", "valid/sa/out/017a.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-037
%% Output: valid/sa/out/037.xml
%% Description:
%%   Test demonstrates a valid comment and that it may appear anywhere in
%%   the document including at the end.
'valid-sa-037'(Config) ->
    run_test(Config, "xmltest", "valid/sa/037.xml", "valid", "valid/sa/out/037.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-038
%% Output: valid/sa/out/038.xml
%% Description:
%%   Test demonstrates a valid comment and that it may appear anywhere in
%%   the document including the beginning.
'valid-sa-038'(Config) ->
    run_test(Config, "xmltest", "valid/sa/038.xml", "valid", "valid/sa/out/038.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-039
%% Output: valid/sa/out/039.xml
%% Description:
%%   Test demonstrates a valid processing instruction and that it may
%%   appear at the beginning of the document.
'valid-sa-039'(Config) ->
    run_test(Config, "xmltest", "valid/sa/039.xml", "valid", "valid/sa/out/039.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-040
%% Output: valid/sa/out/040.xml
%% Description:
%%   Test demonstrates an Attribute List declaration that uses a
%%   StringType as the AttType.
'valid-sa-040'(Config) ->
    run_test(Config, "xmltest", "valid/sa/040.xml", "valid", "valid/sa/out/040.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-041
%% Output: valid/sa/out/041.xml
%% Description:
%%   Test demonstrates an Attribute List declaration that uses a
%%   StringType as the AttType and also expands the CDATA attribute with
%%   a character reference.
'valid-sa-041'(Config) ->
    run_test(Config, "xmltest", "valid/sa/041.xml", "valid", "valid/sa/out/041.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-042
%% Output: valid/sa/out/042.xml
%% Description:
%%   Test demonstrates an Attribute List declaration that uses a
%%   StringType as the AttType and also expands the CDATA attribute with
%%   a character reference. The test also shows that the leading zeros in
%%   the character reference are ignored.
'valid-sa-042'(Config) ->
    run_test(Config, "xmltest", "valid/sa/042.xml", "valid", "valid/sa/out/042.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-043
%% Output: valid/sa/out/043.xml
%% Description:
%%   An element's attributes may be declared before its content model;
%%   and attribute values may contain newlines.
'valid-sa-043'(Config) ->
    run_test(Config, "xmltest", "valid/sa/043.xml", "valid", "valid/sa/out/043.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-044
%% Output: valid/sa/out/044.xml
%% Description:
%%   Test demonstrates that the empty-element tag must be use for an
%%   elements that are declared EMPTY.
'valid-sa-044'(Config) ->
    run_test(Config, "xmltest", "valid/sa/044.xml", "valid", "valid/sa/out/044.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-045
%% Output: valid/sa/out/045.xml
%% Description:
%%   Tests whether more than one definition can be provided for the same
%%   attribute of a given element type with the first declaration being
%%   binding.
'valid-sa-045'(Config) ->
    run_test(Config, "xmltest", "valid/sa/045.xml", "valid", "valid/sa/out/045.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-046
%% Output: valid/sa/out/046.xml
%% Description:
%%   Test demonstrates that when more than one AttlistDecl is provided
%%   for a given element type, the contents of all those provided are
%%   merged.
'valid-sa-046'(Config) ->
    run_test(Config, "xmltest", "valid/sa/046.xml", "valid", "valid/sa/out/046.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-047
%% Output: valid/sa/out/047.xml
%% Description:
%%   Test demonstrates that extra whitespace is normalized into single
%%   space character.
'valid-sa-047'(Config) ->
    run_test(Config, "xmltest", "valid/sa/047.xml", "valid", "valid/sa/out/047.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-048
%% Output: valid/sa/out/048.xml
%% Description:
%%   Test demonstrates that character data is valid element content.
'valid-sa-048'(Config) ->
    run_test(Config, "xmltest", "valid/sa/048.xml", "valid", "valid/sa/out/048.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-049
%% Output: valid/sa/out/049.xml
%% Description:
%%   Test demonstrates that characters outside of normal ascii range can
%%   be used as element content.
'valid-sa-049'(Config) ->
    run_test(Config, "xmltest", "valid/sa/049.xml", "valid", "valid/sa/out/049.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-050
%% Output: valid/sa/out/050.xml
%% Description:
%%   Test demonstrates that characters outside of normal ascii range can
%%   be used as element content.
'valid-sa-050'(Config) ->
    run_test(Config, "xmltest", "valid/sa/050.xml", "valid", "valid/sa/out/050.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-051
%% Output: valid/sa/out/051.xml
%% Description:
%%   The document is encoded in UTF-16 and uses some name characters well
%%   outside of the normal ASCII range.
'valid-sa-051'(Config) ->
    run_test(Config, "xmltest", "valid/sa/051.xml", "valid", "valid/sa/out/051.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-052
%% Output: valid/sa/out/052.xml
%% Description:
%%   The document is encoded in UTF-8 and the text inside the root
%%   element uses two non-ASCII characters, encoded in UTF-8 and each of
%%   which expands to a Unicode surrogate pair.
'valid-sa-052'(Config) ->
    run_test(Config, "xmltest", "valid/sa/052.xml", "valid", "valid/sa/out/052.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-053
%% Output: valid/sa/out/053.xml
%% Description:
%%   Tests inclusion of a well-formed internal entity, which holds an
%%   element required by the content model.
'valid-sa-053'(Config) ->
    run_test(Config, "xmltest", "valid/sa/053.xml", "valid", "valid/sa/out/053.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-054
%% Output: valid/sa/out/054.xml
%% Description:
%%   Test demonstrates that extra whitespace within Start-tags and
%%   End-tags are nomalized into single spaces.
'valid-sa-054'(Config) ->
    run_test(Config, "xmltest", "valid/sa/054.xml", "valid", "valid/sa/out/054.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-055
%% Output: valid/sa/out/055.xml
%% Description:
%%   Test demonstrates that extra whitespace within a processing
%%   instruction willnormalized into s single space character.
'valid-sa-055'(Config) ->
    run_test(Config, "xmltest", "valid/sa/055.xml", "valid", "valid/sa/out/055.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-056
%% Output: valid/sa/out/056.xml
%% Description:
%%   Test demonstrates an Attribute List declaration that uses a
%%   StringType as the AttType and also expands the CDATA attribute with
%%   a character reference. The test also shows that the leading zeros in
%%   the character reference are ignored.
'valid-sa-056'(Config) ->
    run_test(Config, "xmltest", "valid/sa/056.xml", "valid", "valid/sa/out/056.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-057
%% Output: valid/sa/out/057.xml
%% Description:
%%   Test demonstrates an element content model whose element can occur
%%   zero or more times.
'valid-sa-057'(Config) ->
    run_test(Config, "xmltest", "valid/sa/057.xml", "valid", "valid/sa/out/057.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-058
%% Output: valid/sa/out/058.xml
%% Description:
%%   Test demonstrates that extra whitespace be normalized into a single
%%   space character in an attribute of type NMTOKENS.
'valid-sa-058'(Config) ->
    run_test(Config, "xmltest", "valid/sa/058.xml", "valid", "valid/sa/out/058.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-059
%% Output: valid/sa/out/059.xml
%% Description:
%%   Test demonstrates an Element Type Declaration that uses the
%%   contentspec of EMPTY. The element cannot have any contents and must
%%   always appear as an empty element in the document. The test also
%%   shows an Attribute-list declaration with multiple AttDef's.
'valid-sa-059'(Config) ->
    run_test(Config, "xmltest", "valid/sa/059.xml", "valid", "valid/sa/out/059.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-060
%% Output: valid/sa/out/060.xml
%% Description:
%%   Test demonstrates the use of decimal Character References within
%%   element content.
'valid-sa-060'(Config) ->
    run_test(Config, "xmltest", "valid/sa/060.xml", "valid", "valid/sa/out/060.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-061
%% Output: valid/sa/out/061.xml
%% Description:
%%   Test demonstrates the use of decimal Character References within
%%   element content.
'valid-sa-061'(Config) ->
    run_test(Config, "xmltest", "valid/sa/061.xml", "valid", "valid/sa/out/061.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-062
%% Output: valid/sa/out/062.xml
%% Description:
%%   Test demonstrates the use of hexadecimal Character References within
%%   element.
'valid-sa-062'(Config) ->
    run_test(Config, "xmltest", "valid/sa/062.xml", "valid", "valid/sa/out/062.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-063
%% Output: valid/sa/out/063.xml
%% Description:
%%   The document is encoded in UTF-8 and the name of the root element
%%   type uses non-ASCII characters.
'valid-sa-063'(Config) ->
    run_test(Config, "xmltest", "valid/sa/063.xml", "valid", "valid/sa/out/063.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-064
%% Output: valid/sa/out/064.xml
%% Description:
%%   Tests in-line handling of two legal character references, which each
%%   expand to a Unicode surrogate pair.
'valid-sa-064'(Config) ->
    run_test(Config, "xmltest", "valid/sa/064.xml", "valid", "valid/sa/out/064.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-065
%% Output: valid/sa/out/065.xml
%% Description:
%%   Tests ability to define an internal entity which can't legally be
%%   expanded (contains an unquoted ).
'valid-sa-065'(Config) ->
    run_test(Config, "xmltest", "valid/sa/065.xml", "valid", "valid/sa/out/065.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-066
%% Output: valid/sa/out/066.xml
%% Description:
%%   Expands a CDATA attribute with a character reference.
'valid-sa-066'(Config) ->
    run_test(Config, "xmltest", "valid/sa/066.xml", "valid", "valid/sa/out/066.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-067
%% Output: valid/sa/out/067.xml
%% Description:
%%   Test demonstrates the use of decimal character references within
%%   element content.
'valid-sa-067'(Config) ->
    run_test(Config, "xmltest", "valid/sa/067.xml", "valid", "valid/sa/out/067.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-068
%% Output: valid/sa/out/068.xml
%% Description:
%%   Tests definition of an internal entity holding a carriage return
%%   character reference, which must not be normalized before reporting
%%   to the application. Line break normalization only occurs when
%%   parsing external parsed entities.
'valid-sa-068'(Config) ->
    run_test(Config, "xmltest", "valid/sa/068.xml", "valid", "valid/sa/out/068.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-069
%% Output: valid/sa/out/069.xml
%% Description:
%%   Verifies that an XML parser will parse a NOTATION declaration; the
%%   output phase of this test ensures that it's reported to the
%%   application.
'valid-sa-069'(Config) ->
    run_test(Config, "xmltest", "valid/sa/069.xml", "valid", "valid/sa/out/069.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-070
%% Entities: parameter
%% Output: valid/sa/out/070.xml
%% Description:
%%   Verifies that internal parameter entities are correctly expanded
%%   within the internal subset.
'valid-sa-070'(Config) ->
    run_test(Config, "xmltest", "valid/sa/070.xml", "valid", "valid/sa/out/070.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-071
%% Output: valid/sa/out/071.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use ID as the
%%   TokenizedType within the Attribute type. The test also shows that
%%   IMPLIED is a valid DefaultDecl.
'valid-sa-071'(Config) ->
    run_test(Config, "xmltest", "valid/sa/071.xml", "valid", "valid/sa/out/071.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-072
%% Output: valid/sa/out/072.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use IDREF as the
%%   TokenizedType within the Attribute type. The test also shows that
%%   IMPLIED is a valid DefaultDecl.
'valid-sa-072'(Config) ->
    run_test(Config, "xmltest", "valid/sa/072.xml", "valid", "valid/sa/out/072.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-073
%% Output: valid/sa/out/073.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use IDREFS as the
%%   TokenizedType within the Attribute type. The test also shows that
%%   IMPLIED is a valid DefaultDecl.
'valid-sa-073'(Config) ->
    run_test(Config, "xmltest", "valid/sa/073.xml", "valid", "valid/sa/out/073.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-074
%% Output: valid/sa/out/074.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use ENTITY as the
%%   TokenizedType within the Attribute type. The test also shows that
%%   IMPLIED is a valid DefaultDecl.
'valid-sa-074'(Config) ->
    run_test(Config, "xmltest", "valid/sa/074.xml", "valid", "valid/sa/out/074.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-075
%% Output: valid/sa/out/075.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use ENTITIES as the
%%   TokenizedType within the Attribute type. The test also shows that
%%   IMPLIED is a valid DefaultDecl.
'valid-sa-075'(Config) ->
    run_test(Config, "xmltest", "valid/sa/075.xml", "valid", "valid/sa/out/075.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-076
%% Output: valid/sa/out/076.xml
%% Description:
%%   Verifies that an XML parser will parse a NOTATION attribute; the
%%   output phase of this test ensures that both notations are reported
%%   to the application.
'valid-sa-076'(Config) ->
    run_test(Config, "xmltest", "valid/sa/076.xml", "valid", "valid/sa/out/076.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-077
%% Output: valid/sa/out/077.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use an EnumeratedType
%%   within the Attribute type. The test also shows that IMPLIED is a
%%   valid DefaultDecl.
'valid-sa-077'(Config) ->
    run_test(Config, "xmltest", "valid/sa/077.xml", "valid", "valid/sa/out/077.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-078
%% Output: valid/sa/out/078.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use an StringType of CDATA
%%   within the Attribute type. The test also shows that REQUIRED is a
%%   valid DefaultDecl.
'valid-sa-078'(Config) ->
    run_test(Config, "xmltest", "valid/sa/078.xml", "valid", "valid/sa/out/078.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-079
%% Output: valid/sa/out/079.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use an StringType of CDATA
%%   within the Attribute type. The test also shows that FIXED is a valid
%%   DefaultDecl and that a value can be given to the attribute in the
%%   Start-tag as well as the AttListDecl.
'valid-sa-079'(Config) ->
    run_test(Config, "xmltest", "valid/sa/079.xml", "valid", "valid/sa/out/079.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-080
%% Output: valid/sa/out/080.xml
%% Description:
%%   Test demonstrates that an AttlistDecl can use an StringType of CDATA
%%   within the Attribute type. The test also shows that FIXED is a valid
%%   DefaultDecl and that an value can be given to the attribute.
'valid-sa-080'(Config) ->
    run_test(Config, "xmltest", "valid/sa/080.xml", "valid", "valid/sa/out/080.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-081
%% Output: valid/sa/out/081.xml
%% Description:
%%   Test demonstrates the use of the optional character following a name
%%   or list to govern the number of times an element or content
%%   particles in the list occur.
'valid-sa-081'(Config) ->
    run_test(Config, "xmltest", "valid/sa/081.xml", "valid", "valid/sa/out/081.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-082
%% Output: valid/sa/out/082.xml
%% Description:
%%   Tests that an external PE may be defined (but not referenced).
'valid-sa-082'(Config) ->
    run_test(Config, "xmltest", "valid/sa/082.xml", "valid", "valid/sa/out/082.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-083
%% Output: valid/sa/out/083.xml
%% Description:
%%   Tests that an external PE may be defined (but not referenced).
'valid-sa-083'(Config) ->
    run_test(Config, "xmltest", "valid/sa/083.xml", "valid", "valid/sa/out/083.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-084
%% Output: valid/sa/out/084.xml
%% Description:
%%   Test demonstrates that although whitespace can be used to set apart
%%   markup for greater readability it is not necessary.
'valid-sa-084'(Config) ->
    run_test(Config, "xmltest", "valid/sa/084.xml", "valid", "valid/sa/out/084.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-085
%% Output: valid/sa/out/085.xml
%% Description:
%%   Parameter and General entities use different namespaces, so there
%%   can be an entity of each type with a given name.
'valid-sa-085'(Config) ->
    run_test(Config, "xmltest", "valid/sa/085.xml", "valid", "valid/sa/out/085.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-086
%% Output: valid/sa/out/086.xml
%% Description:
%%   Tests whether entities may be declared more than once, with the
%%   first declaration being the binding one.
'valid-sa-086'(Config) ->
    run_test(Config, "xmltest", "valid/sa/086.xml", "valid", "valid/sa/out/086.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-087
%% Output: valid/sa/out/087.xml
%% Description:
%%   Tests whether character references in internal entities are expanded
%%   early enough, by relying on correct handling to make the entity be
%%   well formed.
'valid-sa-087'(Config) ->
    run_test(Config, "xmltest", "valid/sa/087.xml", "valid", "valid/sa/out/087.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-088
%% Output: valid/sa/out/088.xml
%% Description:
%%   Tests whether entity references in internal entities are expanded
%%   late enough, by relying on correct handling to make the expanded
%%   text be valid. (If it's expanded too early, the entity will parse as
%%   an element that's not valid in that context.)
'valid-sa-088'(Config) ->
    run_test(Config, "xmltest", "valid/sa/088.xml", "valid", "valid/sa/out/088.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-089
%% Output: valid/sa/out/089.xml
%% Description:
%%   Tests entity expansion of three legal character references, which
%%   each expand to a Unicode surrogate pair.
'valid-sa-089'(Config) ->
    run_test(Config, "xmltest", "valid/sa/089.xml", "valid", "valid/sa/out/089.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-090
%% Output: valid/sa/out/090.xml
%% Description:
%%   Verifies that an XML parser will parse a NOTATION attribute; the
%%   output phase of this test ensures that the notation is reported to
%%   the application.
'valid-sa-090'(Config) ->
    run_test(Config, "xmltest", "valid/sa/090.xml", "valid", "valid/sa/out/090.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-091
%% Output: valid/sa/out/091.xml
%% Description:
%%   Verifies that an XML parser will parse an ENTITY attribute; the
%%   output phase of this test ensures that the notation is reported to
%%   the application, and for validating parsers it further tests that
%%   the entity is so reported.
'valid-sa-091'(Config) ->
    run_test(Config, "xmltest", "valid/sa/091.xml", "valid", "valid/sa/out/091.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-092
%% Output: valid/sa/out/092.xml
%% Description:
%%   Test demostrates that extra whitespace is normalized into a single
%%   space character.
'valid-sa-092'(Config) ->
    run_test(Config, "xmltest", "valid/sa/092.xml", "valid", "valid/sa/out/092.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-093
%% Output: valid/sa/out/093.xml
%% Description:
%%   Test demonstrates that extra whitespace is not intended for
%%   inclusion in the delivered version of the document.
'valid-sa-093'(Config) ->
    run_test(Config, "xmltest", "valid/sa/093.xml", "valid", "valid/sa/out/093.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-094
%% Output: valid/sa/out/094.xml
%% Description:
%%   Attribute defaults with a DTD have special parsing rules, different
%%   from other strings. That means that characters found there may look
%%   like an undefined parameter entity reference "within a markup
%%   declaration", but they aren't ... so they can't be violating the
%%   WFC.
'valid-sa-094'(Config) ->
    run_test(Config, "xmltest", "valid/sa/094.xml", "valid", "valid/sa/out/094.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-095
%% Output: valid/sa/out/095.xml
%% Description:
%%   Basically an output test, this requires extra whitespace to be
%%   normalized into a single space character in an attribute of type
%%   NMTOKENS.
'valid-sa-095'(Config) ->
    run_test(Config, "xmltest", "valid/sa/095.xml", "valid", "valid/sa/out/095.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-096
%% Output: valid/sa/out/096.xml
%% Description:
%%   Test demonstrates that extra whitespace is normalized into a single
%%   space character in an attribute of type NMTOKENS.
'valid-sa-096'(Config) ->
    run_test(Config, "xmltest", "valid/sa/096.xml", "valid", "valid/sa/out/096.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-097
%% Entities: parameter
%% Output: valid/sa/out/097.xml
%% Description:
%%   Basically an output test, this tests whether an externally defined
%%   attribute declaration (with a default) takes proper precedence over
%%   a subsequent internal declaration.
'valid-sa-097'(Config) ->
    run_test(Config, "xmltest", "valid/sa/097.xml", "valid", "valid/sa/out/097.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-098
%% Output: valid/sa/out/098.xml
%% Description:
%%   Test demonstrates that extra whitespace within a processing
%%   instruction is converted into a single space character.
'valid-sa-098'(Config) ->
    run_test(Config, "xmltest", "valid/sa/098.xml", "valid", "valid/sa/out/098.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-099
%% Output: valid/sa/out/099.xml
%% Description:
%%   Test demonstrates the name of the encoding can be composed of
%%   lowercase characters.
'valid-sa-099'(Config) ->
    run_test(Config, "xmltest", "valid/sa/099.xml", "valid", "valid/sa/out/099.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-100
%% Output: valid/sa/out/100.xml
%% Description:
%%   Makes sure that PUBLIC identifiers may have some strange characters.
'valid-sa-100'(Config) ->
    run_test(Config, "xmltest", "valid/sa/100.xml", "valid", "valid/sa/out/100.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-101
%% Output: valid/sa/out/101.xml
%% Description:
%%   This tests whether entity expansion is (incorrectly) done while
%%   processing entity declarations; if it is, the entity value literal
%%   will terminate prematurely.
'valid-sa-101'(Config) ->
    run_test(Config, "xmltest", "valid/sa/101.xml", "valid", "valid/sa/out/101.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-102
%% Output: valid/sa/out/102.xml
%% Description:
%%   Test demonstrates that a CDATA attribute can pass a double quote as
%%   its value.
'valid-sa-102'(Config) ->
    run_test(Config, "xmltest", "valid/sa/102.xml", "valid", "valid/sa/out/102.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-103
%% Output: valid/sa/out/103.xml
%% Description:
%%   Test demonstrates that an attribute can pass a less than sign as its
%%   value.
'valid-sa-103'(Config) ->
    run_test(Config, "xmltest", "valid/sa/103.xml", "valid", "valid/sa/out/103.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-104
%% Output: valid/sa/out/104.xml
%% Description:
%%   Test demonstrates that extra whitespace within an Attribute of a
%%   Start-tag is normalized to a single space character.
'valid-sa-104'(Config) ->
    run_test(Config, "xmltest", "valid/sa/104.xml", "valid", "valid/sa/out/104.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-105
%% Output: valid/sa/out/105.xml
%% Description:
%%   Basically an output test, this requires a CDATA attribute with a tab
%%   character to be passed through as one space.
'valid-sa-105'(Config) ->
    run_test(Config, "xmltest", "valid/sa/105.xml", "valid", "valid/sa/out/105.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-106
%% Output: valid/sa/out/106.xml
%% Description:
%%   Basically an output test, this requires a CDATA attribute with a
%%   newline character to be passed through as one space.
'valid-sa-106'(Config) ->
    run_test(Config, "xmltest", "valid/sa/106.xml", "valid", "valid/sa/out/106.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-107
%% Output: valid/sa/out/107.xml
%% Description:
%%   Basically an output test, this requires a CDATA attribute with a
%%   return character to be passed through as one space.
'valid-sa-107'(Config) ->
    run_test(Config, "xmltest", "valid/sa/107.xml", "valid", "valid/sa/out/107.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-108
%% Output: valid/sa/out/108.xml
%% Description:
%%   This tests normalization of end-of-line characters (CRLF) within
%%   entities to LF, primarily as an output test.
'valid-sa-108'(Config) ->
    run_test(Config, "xmltest", "valid/sa/108.xml", "valid", "valid/sa/out/108.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-109
%% Output: valid/sa/out/109.xml
%% Description:
%%   Test demonstrates that an attribute can have a null value.
'valid-sa-109'(Config) ->
    run_test(Config, "xmltest", "valid/sa/109.xml", "valid", "valid/sa/out/109.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-110
%% Output: valid/sa/out/110.xml
%% Description:
%%   Basically an output test, this requires that a CDATA attribute with
%%   a CRLF be normalized to one space.
'valid-sa-110'(Config) ->
    run_test(Config, "xmltest", "valid/sa/110.xml", "valid", "valid/sa/out/110.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-111
%% Output: valid/sa/out/111.xml
%% Description:
%%   Character references expanding to spaces doesn't affect treatment of
%%   attributes.
'valid-sa-111'(Config) ->
    run_test(Config, "xmltest", "valid/sa/111.xml", "valid", "valid/sa/out/111.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-112
%% Output: valid/sa/out/112.xml
%% Description:
%%   Test demonstrates shows the use of content particles within the
%%   element content.
'valid-sa-112'(Config) ->
    run_test(Config, "xmltest", "valid/sa/112.xml", "valid", "valid/sa/out/112.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-113
%% Output: valid/sa/out/113.xml
%% Description:
%%   Test demonstrates that it is not an error to have attributes
%%   declared for an element not itself declared.
'valid-sa-113'(Config) ->
    run_test(Config, "xmltest", "valid/sa/113.xml", "valid", "valid/sa/out/113.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-114
%% Output: valid/sa/out/114.xml
%% Description:
%%   Test demonstrates that all text within a valid CDATA section is
%%   considered text and not recognized as markup.
'valid-sa-114'(Config) ->
    run_test(Config, "xmltest", "valid/sa/114.xml", "valid", "valid/sa/out/114.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-115
%% Output: valid/sa/out/115.xml
%% Description:
%%   Test demonstrates that an entity reference is processed by
%%   recursively processing the replacement text of the entity.
'valid-sa-115'(Config) ->
    run_test(Config, "xmltest", "valid/sa/115.xml", "valid", "valid/sa/out/115.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-116
%% Output: valid/sa/out/116.xml
%% Description:
%%   Test demonstrates that a line break within CDATA will be normalized.
'valid-sa-116'(Config) ->
    run_test(Config, "xmltest", "valid/sa/116.xml", "valid", "valid/sa/out/116.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-117
%% Output: valid/sa/out/117.xml
%% Description:
%%   Test demonstrates that entity expansion is done while processing
%%   entity declarations.
'valid-sa-117'(Config) ->
    run_test(Config, "xmltest", "valid/sa/117.xml", "valid", "valid/sa/out/117.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-118
%% Output: valid/sa/out/118.xml
%% Description:
%%   Test demonstrates that entity expansion is done while processing
%%   entity declarations.
'valid-sa-118'(Config) ->
    run_test(Config, "xmltest", "valid/sa/118.xml", "valid", "valid/sa/out/118.xml").

%%----------------------------------------------------------------------
%% ID: valid-sa-119
%% Output: valid/sa/out/119.xml
%% Description:
%%   Comments may contain any legal XML characters; only the string "--"
%%   is disallowed.
'valid-sa-119'(Config) ->
    run_test(Config, "xmltest", "valid/sa/119.xml", "valid", "valid/sa/out/119.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-001
%% Entities: both
%% Output: valid/not-sa/out/001.xml
%% Description:
%%   Test demonstrates the use of an ExternalID within a document type
%%   definition.
'valid-not-sa-001'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/001.xml", "valid", "valid/not-sa/out/001.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-002
%% Entities: both
%% Output: valid/not-sa/out/002.xml
%% Description:
%%   Test demonstrates the use of an ExternalID within a document type
%%   definition.
'valid-not-sa-002'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/002.xml", "valid", "valid/not-sa/out/002.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-003
%% Entities: both
%% Output: valid/not-sa/out/003.xml
%% Description:
%%   Test demonstrates the expansion of an external parameter entity that
%%   declares an attribute.
'valid-not-sa-003'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/003.xml", "valid", "valid/not-sa/out/003.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-004
%% Entities: both
%% Output: valid/not-sa/out/004.xml
%% Description:
%%   Expands an external parameter entity in two different ways, with one
%%   of them declaring an attribute.
'valid-not-sa-004'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/004.xml", "valid", "valid/not-sa/out/004.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-005
%% Entities: both
%% Output: valid/not-sa/out/005.xml
%% Description:
%%   Test demonstrates the expansion of an external parameter entity that
%%   declares an attribute.
'valid-not-sa-005'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/005.xml", "valid", "valid/not-sa/out/005.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-006
%% Entities: both
%% Output: valid/not-sa/out/006.xml
%% Description:
%%   Test demonstrates that when more than one definition is provided for
%%   the same attribute of a given element type only the first
%%   declaration is binding.
'valid-not-sa-006'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/006.xml", "valid", "valid/not-sa/out/006.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-007
%% Entities: both
%% Output: valid/not-sa/out/007.xml
%% Description:
%%   Test demonstrates the use of an Attribute list declaration within an
%%   external entity.
'valid-not-sa-007'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/007.xml", "valid", "valid/not-sa/out/007.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-008
%% Entities: both
%% Output: valid/not-sa/out/008.xml
%% Description:
%%   Test demonstrates that an external identifier may include a public
%%   identifier.
'valid-not-sa-008'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/008.xml", "valid", "valid/not-sa/out/008.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-009
%% Entities: both
%% Output: valid/not-sa/out/009.xml
%% Description:
%%   Test demonstrates that an external identifier may include a public
%%   identifier.
'valid-not-sa-009'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/009.xml", "valid", "valid/not-sa/out/009.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-010
%% Entities: both
%% Output: valid/not-sa/out/010.xml
%% Description:
%%   Test demonstrates that when more that one definition is provided for
%%   the same attribute of a given element type only the first
%%   declaration is binding.
'valid-not-sa-010'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/010.xml", "valid", "valid/not-sa/out/010.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-011
%% Entities: both
%% Output: valid/not-sa/out/011.xml
%% Description:
%%   Test demonstrates a parameter entity declaration whose parameter
%%   entity definition is an ExternalID.
'valid-not-sa-011'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/011.xml", "valid", "valid/not-sa/out/011.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-012
%% Entities: both
%% Output: valid/not-sa/out/012.xml
%% Description:
%%   Test demonstrates an enternal parsed entity that begins with a text
%%   declaration.
'valid-not-sa-012'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/012.xml", "valid", "valid/not-sa/out/012.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-013
%% Entities: both
%% Output: valid/not-sa/out/013.xml
%% Description:
%%   Test demonstrates the use of the conditional section INCLUDE that
%%   will include its contents as part of the DTD.
'valid-not-sa-013'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/013.xml", "valid", "valid/not-sa/out/013.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-014
%% Entities: both
%% Output: valid/not-sa/out/014.xml
%% Description:
%%   Test demonstrates the use of the conditional section INCLUDE that
%%   will include its contents as part of the DTD. The keyword is a
%%   parameter-entity reference.
'valid-not-sa-014'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/014.xml", "valid", "valid/not-sa/out/014.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-015
%% Entities: both
%% Output: valid/not-sa/out/015.xml
%% Description:
%%   Test demonstrates the use of the conditonal section IGNORE the will
%%   ignore its content from being part of the DTD. The keyword is a
%%   parameter-entity reference.
'valid-not-sa-015'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/015.xml", "valid", "valid/not-sa/out/015.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-016
%% Entities: both
%% Output: valid/not-sa/out/016.xml
%% Description:
%%   Test demonstrates the use of the conditional section INCLUDE that
%%   will include its contents as part of the DTD. The keyword is a
%%   parameter-entity reference.
'valid-not-sa-016'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/016.xml", "valid", "valid/not-sa/out/016.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-017
%% Entities: both
%% Output: valid/not-sa/out/017.xml
%% Description:
%%   Test demonstrates a parameter entity declaration that contains an
%%   attribute list declaration.
'valid-not-sa-017'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/017.xml", "valid", "valid/not-sa/out/017.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-018
%% Entities: both
%% Output: valid/not-sa/out/018.xml
%% Description:
%%   Test demonstrates an EnternalID whose contents contain an parameter
%%   entity declaration and a attribute list definition.
'valid-not-sa-018'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/018.xml", "valid", "valid/not-sa/out/018.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-019
%% Entities: both
%% Output: valid/not-sa/out/019.xml
%% Description:
%%   Test demonstrates that a parameter entity will be expanded with
%%   spaces on either side.
'valid-not-sa-019'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/019.xml", "valid", "valid/not-sa/out/019.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-020
%% Entities: both
%% Output: valid/not-sa/out/020.xml
%% Description:
%%   Parameter entities expand with spaces on either side.
'valid-not-sa-020'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/020.xml", "valid", "valid/not-sa/out/020.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-021
%% Entities: both
%% Output: valid/not-sa/out/021.xml
%% Description:
%%   Test demonstrates a parameter entity declaration that contains a
%%   partial attribute list declaration.
'valid-not-sa-021'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/021.xml", "valid", "valid/not-sa/out/021.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-023
%% Entities: both
%% Output: valid/not-sa/out/023.xml
%% Description:
%%   Test demonstrates the use of a parameter entity reference within an
%%   attribute list declaration.
'valid-not-sa-023'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/023.xml", "valid", "valid/not-sa/out/023.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-024
%% Entities: both
%% Output: valid/not-sa/out/024.xml
%% Description:
%%   Constructs an <!ATTLIST...> declaration from several PEs.
'valid-not-sa-024'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/024.xml", "valid", "valid/not-sa/out/024.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-025
%% Entities: both
%% Output: valid/not-sa/out/025.xml
%% Description:
%%   Test demonstrates that when more that one definition is provided for
%%   the same entity only the first declaration is binding.
'valid-not-sa-025'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/025.xml", "valid", "valid/not-sa/out/025.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-026
%% Entities: both
%% Output: valid/not-sa/out/026.xml
%% Description:
%%   Test demonstrates that when more that one definition is provided for
%%   the same attribute of a given element type only the first
%%   declaration is binding.
'valid-not-sa-026'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/026.xml", "valid", "valid/not-sa/out/026.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-027
%% Entities: both
%% Output: valid/not-sa/out/027.xml
%% Description:
%%   Test demonstrates a parameter entity reference whose value is NULL.
'valid-not-sa-027'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/027.xml", "valid", "valid/not-sa/out/027.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-028
%% Entities: both
%% Output: valid/not-sa/out/028.xml
%% Description:
%%   Test demonstrates the use of the conditional section INCLUDE that
%%   will include its contents.
'valid-not-sa-028'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/028.xml", "valid", "valid/not-sa/out/028.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-029
%% Entities: both
%% Output: valid/not-sa/out/029.xml
%% Description:
%%   Test demonstrates the use of the conditonal section IGNORE the will
%%   ignore its content from being used.
'valid-not-sa-029'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/029.xml", "valid", "valid/not-sa/out/029.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-030
%% Entities: both
%% Output: valid/not-sa/out/030.xml
%% Description:
%%   Test demonstrates the use of the conditonal section IGNORE the will
%%   ignore its content from being used.
'valid-not-sa-030'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/030.xml", "valid", "valid/not-sa/out/030.xml").

%%----------------------------------------------------------------------
%% ID: valid-not-sa-031
%% Entities: both
%% Output: valid/not-sa/out/031.xml
%% Description:
%%   Expands a general entity which contains a CDATA section with what
%%   looks like a markup declaration (but is just text since it's in a
%%   CDATA section).
'valid-not-sa-031'(Config) ->
    run_test(Config, "xmltest", "valid/not-sa/031.xml", "valid", "valid/not-sa/out/031.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-001
%% Entities: both
%% Output: valid/ext-sa/out/001.xml
%% Description:
%%   A combination of carriage return line feed in an external entity
%%   must be normalized to a single newline.
'valid-ext-sa-001'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/001.xml", "valid", "valid/ext-sa/out/001.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-002
%% Entities: both
%% Output: valid/ext-sa/out/002.xml
%% Description:
%%   A carriage return (also CRLF) in an external entity must be
%%   normalized to a single newline.
'valid-ext-sa-002'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/002.xml", "valid", "valid/ext-sa/out/002.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-003
%% Entities: both
%% Output: valid/ext-sa/out/003.xml
%% Description:
%%   Test demonstrates that the content of an element can be empty. In
%%   this case the external entity is an empty file.
'valid-ext-sa-003'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/003.xml", "valid", "valid/ext-sa/out/003.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-004
%% Entities: both
%% Output: valid/ext-sa/out/004.xml
%% Description:
%%   A carriage return (also CRLF) in an external entity must be
%%   normalized to a single newline.
'valid-ext-sa-004'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/004.xml", "valid", "valid/ext-sa/out/004.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-005
%% Entities: both
%% Output: valid/ext-sa/out/005.xml
%% Description:
%%   Test demonstrates the use of optional character and content
%%   particles within an element content. The test also show the use of
%%   external entity.
'valid-ext-sa-005'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/005.xml", "valid", "valid/ext-sa/out/005.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-006
%% Entities: both
%% Output: valid/ext-sa/out/006.xml
%% Description:
%%   Test demonstrates the use of optional character and content
%%   particles within mixed element content. The test also shows the use
%%   of an external entity and that a carriage control line feed in an
%%   external entity must be normalized to a single newline.
'valid-ext-sa-006'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/006.xml", "valid", "valid/ext-sa/out/006.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-007
%% Entities: both
%% Output: valid/ext-sa/out/007.xml
%% Description:
%%   Test demonstrates the use of external entity and how replacement
%%   text is retrieved and processed.
'valid-ext-sa-007'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/007.xml", "valid", "valid/ext-sa/out/007.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-008
%% Entities: both
%% Output: valid/ext-sa/out/008.xml
%% Description:
%%   Test demonstrates the use of external entity and how replacement
%%   text is retrieved and processed. Also tests the use of an
%%   EncodingDecl of UTF-16.
'valid-ext-sa-008'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/008.xml", "valid", "valid/ext-sa/out/008.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-009
%% Entities: both
%% Output: valid/ext-sa/out/009.xml
%% Description:
%%   A carriage return (also CRLF) in an external entity must be
%%   normalized to a single newline.
'valid-ext-sa-009'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/009.xml", "valid", "valid/ext-sa/out/009.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-011
%% Entities: both
%% Output: valid/ext-sa/out/011.xml
%% Description:
%%   Test demonstrates the use of a public identifier with and external
%%   entity. The test also show that a carriage control line feed
%%   combination in an external entity must be normalized to a single
%%   newline.
'valid-ext-sa-011'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/011.xml", "valid", "valid/ext-sa/out/011.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-012
%% Entities: both
%% Output: valid/ext-sa/out/012.xml
%% Description:
%%   Test demonstrates both internal and external entities and that
%%   processing of entity references may be required to produce the
%%   correct replacement text.
'valid-ext-sa-012'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/012.xml", "valid", "valid/ext-sa/out/012.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-013
%% Entities: both
%% Output: valid/ext-sa/out/013.xml
%% Description:
%%   Test demonstrates that whitespace is handled by adding a single
%%   whitespace to the normalized value in the attribute list.
'valid-ext-sa-013'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/013.xml", "valid", "valid/ext-sa/out/013.xml").

%%----------------------------------------------------------------------
%% ID: valid-ext-sa-014
%% Entities: both
%% Output: valid/ext-sa/out/014.xml
%% Description:
%%   Test demonstrates use of characters outside of normal ASCII range.
'valid-ext-sa-014'(Config) ->
    run_test(Config, "xmltest", "valid/ext-sa/014.xml", "valid", "valid/ext-sa/out/014.xml").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: Fuji Xerox Japanese Text Tests XML 1.0 Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: pr-xml-euc-jp
%% Entities: parameter
%% Description:
%%   Test support for the EUC-JP encoding, and for text which relies on
%%   Japanese characters. (Also requires ability to process a moderately
%%   complex DTD.)
'pr-xml-euc-jp'(Config) -> run_test(Config, "japanese", "pr-xml-euc-jp.xml", "error").

%%----------------------------------------------------------------------
%% ID: pr-xml-iso-2022-jp
%% Entities: parameter
%% Description:
%%   Test support for the ISO-2022-JP encoding, and for text which relies
%%   on Japanese characters. (Also requires ability to process a
%%   moderately complex DTD.)
'pr-xml-iso-2022-jp'(Config) -> run_test(Config, "japanese", "pr-xml-iso-2022-jp.xml", "error").

%%----------------------------------------------------------------------
%% ID: pr-xml-little
%% Entities: parameter
%% Description:
%%   Test support for little-endian UTF-16 text which relies on Japanese
%%   characters. (Also requires ability to process a moderately complex
%%   DTD.)
'pr-xml-little'(Config) -> run_test(Config, "japanese", "pr-xml-little-endian.xml", "valid").

%%----------------------------------------------------------------------
%% ID: pr-xml-shift_jis
%% Entities: parameter
%% Description:
%%   Test support for the Shift_JIS encoding, and for text which relies
%%   on Japanese characters. (Also requires ability to process a
%%   moderately complex DTD.)
'pr-xml-shift_jis'(Config) -> run_test(Config, "japanese", "pr-xml-shift_jis.xml", "error").

%%----------------------------------------------------------------------
%% ID: pr-xml-utf-16
%% Entities: parameter
%% Description:
%%   Test support UTF-16 text which relies on Japanese characters. (Also
%%   requires ability to process a moderately complex DTD.)
'pr-xml-utf-16'(Config) -> run_test(Config, "japanese", "pr-xml-utf-16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: pr-xml-utf-8
%% Entities: parameter
%% Description:
%%   Test support for UTF-8 text which relies on Japanese characters.
%%   (Also requires ability to process a moderately complex DTD.)
'pr-xml-utf-8'(Config) -> run_test(Config, "japanese", "pr-xml-utf-8.xml", "valid").

%%----------------------------------------------------------------------
%% ID: weekly-euc-jp
%% Entities: parameter
%% Description:
%%   Test support for EUC-JP encoding, and XML names which contain
%%   Japanese characters.
'weekly-euc-jp'(Config) -> run_test(Config, "japanese", "weekly-euc-jp.xml", "error").

%%----------------------------------------------------------------------
%% ID: weekly-iso-2022-jp
%% Entities: parameter
%% Description:
%%   Test support for ISO-2022-JP encoding, and XML names which contain
%%   Japanese characters.
'weekly-iso-2022-jp'(Config) -> run_test(Config, "japanese", "weekly-iso-2022-jp.xml", "error").

%%----------------------------------------------------------------------
%% ID: weekly-little
%% Entities: parameter
%% Description:
%%   Test support for little-endian UTF-16 encoding, and XML names which
%%   contain Japanese characters.
'weekly-little'(Config) -> run_test(Config, "japanese", "weekly-little-endian.xml", "valid").

%%----------------------------------------------------------------------
%% ID: weekly-shift_jis
%% Entities: parameter
%% Description:
%%   Test support for Shift_JIS encoding, and XML names which contain
%%   Japanese characters.
'weekly-shift_jis'(Config) -> run_test(Config, "japanese", "weekly-shift_jis.xml", "error").

%%----------------------------------------------------------------------
%% ID: weekly-utf-16
%% Entities: parameter
%% Description:
%%   Test support for UTF-16 encoding, and XML names which contain
%%   Japanese characters.
'weekly-utf-16'(Config) -> run_test(Config, "japanese", "weekly-utf-16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: weekly-utf-8
%% Entities: parameter
%% Description:
%%   Test support for UTF-8 encoding and XML names which contain Japanese
%%   characters.
'weekly-utf-8'(Config) -> run_test(Config, "japanese", "weekly-utf-8.xml", "valid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: Sun Microsystems XML Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: pe01
%% Entities: parameter
%% Description:
%%   Parameter entities references are NOT RECOGNIZED in default
%%   attribute values.
pe01(Config) -> run_test(Config, "sun", "valid/pe01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: dtd00
%% Output: valid/out/dtd00.xml
%% Description:
%%   Tests parsing of alternative forms of text-only mixed content
%%   declaration.
dtd00(Config) -> run_test(Config, "sun", "valid/dtd00.xml", "valid", "valid/out/dtd00.xml").

%%----------------------------------------------------------------------
%% ID: dtd01
%% Output: valid/out/dtd01.xml
%% Description:
%%   Comments don't get parameter entity expansion
dtd01(Config) -> run_test(Config, "sun", "valid/dtd01.xml", "valid", "valid/out/dtd01.xml").

%%----------------------------------------------------------------------
%% ID: element
%% Output: valid/out/element.xml
%% Description:
%%   Tests clauses 1, 3, and 4 of the Element Valid validity constraint.
element(Config) -> run_test(Config, "sun", "valid/element.xml", "valid", "valid/out/element.xml").

%%----------------------------------------------------------------------
%% ID: ext01
%% Entities: general
%% Output: valid/out/ext01.xml
%% Description:
%%   Tests use of external parsed entities with and without content.
ext01(Config) -> run_test(Config, "sun", "valid/ext01.xml", "valid", "valid/out/ext01.xml").

%%----------------------------------------------------------------------
%% ID: ext02
%% Entities: general
%% Output: valid/out/ext02.xml
%% Description:
%%   Tests use of external parsed entities with different encodings than
%%   the base document.
ext02(Config) -> run_test(Config, "sun", "valid/ext02.xml", "valid", "valid/out/ext02.xml").

%%----------------------------------------------------------------------
%% ID: not-sa01
%% Entities: parameter
%% Output: valid/out/not-sa01.xml
%% Description:
%%   A non-standalone document is valid if declared as such.
'not-sa01'(Config) ->
    run_test(Config, "sun", "valid/not-sa01.xml", "valid", "valid/out/not-sa01.xml").

%%----------------------------------------------------------------------
%% ID: not-sa02
%% Entities: parameter
%% Output: valid/out/not-sa02.xml
%% Description:
%%   A non-standalone document is valid if declared as such.
'not-sa02'(Config) ->
    run_test(Config, "sun", "valid/not-sa02.xml", "valid", "valid/out/not-sa02.xml").

%%----------------------------------------------------------------------
%% ID: not-sa03
%% Entities: parameter
%% Output: valid/out/not-sa03.xml
%% Description:
%%   A non-standalone document is valid if declared as such.
'not-sa03'(Config) ->
    run_test(Config, "sun", "valid/not-sa03.xml", "valid", "valid/out/not-sa03.xml").

%%----------------------------------------------------------------------
%% ID: not-sa04
%% Entities: parameter
%% Output: valid/out/not-sa04.xml
%% Description:
%%   A non-standalone document is valid if declared as such.
'not-sa04'(Config) ->
    run_test(Config, "sun", "valid/not-sa04.xml", "valid", "valid/out/not-sa04.xml").

%%----------------------------------------------------------------------
%% ID: notation01
%% Entities: parameter
%% Output: valid/out/notation01.xml
%% Description:
%%   NOTATION declarations don't need SYSTEM IDs; and externally declared
%%   notations may be used to declare unparsed entities in the internal
%%   DTD subset. The notation must be reported to the application.
notation01(Config) ->
    run_test(Config, "sun", "valid/notation01.xml", "valid", "valid/out/notation01.xml").

%%----------------------------------------------------------------------
%% ID: optional
%% Entities: parameter
%% Output: valid/out/optional.xml
%% Description:
%%   Tests declarations of "children" content models, and the validity
%%   constraints associated with them.
optional(Config) ->
    run_test(Config, "sun", "valid/optional.xml", "valid", "valid/out/optional.xml").

%%----------------------------------------------------------------------
%% ID: required00
%% Output: valid/out/required00.xml
%% Description:
%%   Tests the #REQUIRED attribute declaration syntax, and the associated
%%   validity constraint.
required00(Config) ->
    run_test(Config, "sun", "valid/required00.xml", "valid", "valid/out/required00.xml").

%%----------------------------------------------------------------------
%% ID: sa01
%% Output: valid/out/sa01.xml
%% Description:
%%   A document may be marked 'standalone' if any optional whitespace is
%%   defined within the internal DTD subset.
sa01(Config) -> run_test(Config, "sun", "valid/sa01.xml", "valid", "valid/out/sa01.xml").

%%----------------------------------------------------------------------
%% ID: sa02
%% Output: valid/out/sa02.xml
%% Description:
%%   A document may be marked 'standalone' if any attributes that need
%%   normalization are defined within the internal DTD subset.
sa02(Config) -> run_test(Config, "sun", "valid/sa02.xml", "valid", "valid/out/sa02.xml").

%%----------------------------------------------------------------------
%% ID: sa03
%% Entities: parameter
%% Output: valid/out/sa03.xml
%% Description:
%%   A document may be marked 'standalone' if any the defined entities
%%   need expanding are internal, and no attributes need defaulting or
%%   normalization. On output, requires notations to be correctly
%%   reported.
sa03(Config) -> run_test(Config, "sun", "valid/sa03.xml", "valid", "valid/out/sa03.xml").

%%----------------------------------------------------------------------
%% ID: sa04
%% Entities: parameter
%% Output: valid/out/sa04.xml
%% Description:
%%   Like sa03 but relies on attribute defaulting defined in the internal
%%   subset. On output, requires notations to be correctly reported.
sa04(Config) -> run_test(Config, "sun", "valid/sa04.xml", "valid", "valid/out/sa04.xml").

%%----------------------------------------------------------------------
%% ID: sa05
%% Entities: parameter
%% Output: valid/out/sa05.xml
%% Description:
%%   Like sa01 but this document is standalone since it has no optional
%%   whitespace. On output, requires notations to be correctly reported.
sa05(Config) -> run_test(Config, "sun", "valid/sa05.xml", "valid", "valid/out/sa05.xml").

%%----------------------------------------------------------------------
%% ID: v-sgml01
%% Output: valid/out/sgml01.xml
%% Description:
%%   XML permits token reuse, while SGML does not.
'v-sgml01'(Config) -> run_test(Config, "sun", "valid/sgml01.xml", "valid", "valid/out/sgml01.xml").

%%----------------------------------------------------------------------
%% ID: v-lang01
%% Output: valid/out/v-lang01.xml
%% Description:
%%   Tests a lowercase ISO language code.
'v-lang01'(Config) ->
    run_test(Config, "sun", "valid/v-lang01.xml", "valid", "valid/out/v-lang01.xml").

%%----------------------------------------------------------------------
%% ID: v-lang02
%% Output: valid/out/v-lang02.xml
%% Description:
%%   Tests a ISO language code with a subcode.
'v-lang02'(Config) ->
    run_test(Config, "sun", "valid/v-lang02.xml", "valid", "valid/out/v-lang02.xml").

%%----------------------------------------------------------------------
%% ID: v-lang03
%% Output: valid/out/v-lang03.xml
%% Description:
%%   Tests a IANA language code with a subcode.
'v-lang03'(Config) ->
    run_test(Config, "sun", "valid/v-lang03.xml", "valid", "valid/out/v-lang03.xml").

%%----------------------------------------------------------------------
%% ID: v-lang04
%% Output: valid/out/v-lang04.xml
%% Description:
%%   Tests a user language code with a subcode.
'v-lang04'(Config) ->
    run_test(Config, "sun", "valid/v-lang04.xml", "valid", "valid/out/v-lang04.xml").

%%----------------------------------------------------------------------
%% ID: v-lang05
%% Output: valid/out/v-lang05.xml
%% Description:
%%   Tests an uppercase ISO language code.
'v-lang05'(Config) ->
    run_test(Config, "sun", "valid/v-lang05.xml", "valid", "valid/out/v-lang05.xml").

%%----------------------------------------------------------------------
%% ID: v-lang06
%% Output: valid/out/v-lang06.xml
%% Description:
%%   Tests a user language code.
'v-lang06'(Config) ->
    run_test(Config, "sun", "valid/v-lang06.xml", "valid", "valid/out/v-lang06.xml").

%%----------------------------------------------------------------------
%% ID: v-pe00
%% Entities: parameter
%% Output: valid/out/pe00.xml
%% Description:
%%   Tests construction of internal entity replacement text, using an
%%   example in the XML specification.
'v-pe00'(Config) -> run_test(Config, "sun", "valid/pe00.xml", "valid", "valid/out/pe00.xml").

%%----------------------------------------------------------------------
%% ID: v-pe03
%% Output: valid/out/pe03.xml
%% Description:
%%   Tests construction of internal entity replacement text, using an
%%   example in the XML specification.
'v-pe03'(Config) -> run_test(Config, "sun", "valid/pe03.xml", "valid", "valid/out/pe03.xml").

%%----------------------------------------------------------------------
%% ID: v-pe02
%% Entities: parameter
%% Output: valid/out/pe02.xml
%% Description:
%%   Tests construction of internal entity replacement text, using a
%%   complex example in the XML specification.
'v-pe02'(Config) -> run_test(Config, "sun", "valid/pe02.xml", "valid", "valid/out/pe02.xml").

%%----------------------------------------------------------------------
%% ID: inv-dtd01
%% Description:
%%   Tests the No Duplicate Types VC
'inv-dtd01'(Config) -> run_test(Config, "sun", "invalid/dtd01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-dtd02
%% Description:
%%   Tests the "Notation Declared" VC by using an undeclared notation
%%   name.
'inv-dtd02'(Config) -> run_test(Config, "sun", "invalid/dtd02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-dtd03
%% Description:
%%   Tests the "Element Valid" VC (clause 2) by omitting a required
%%   element.
'inv-dtd03'(Config) -> run_test(Config, "sun", "invalid/dtd03.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el01
%% Description:
%%   Tests the Element Valid VC (clause 4) by including an undeclared
%%   child element.
el01(Config) -> run_test(Config, "sun", "invalid/el01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el02
%% Description:
%%   Tests the Element Valid VC (clause 1) by including elements in an
%%   EMPTY content model.
el02(Config) -> run_test(Config, "sun", "invalid/el02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el03
%% Description:
%%   Tests the Element Valid VC (clause 3) by including a child element
%%   not permitted by a mixed content model.
el03(Config) -> run_test(Config, "sun", "invalid/el03.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el04
%% Description:
%%   Tests the Unique Element Type Declaration VC.
el04(Config) -> run_test(Config, "sun", "invalid/el04.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el05
%% Description:
%%   Tests the No Duplicate Types VC.
el05(Config) -> run_test(Config, "sun", "invalid/el05.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: el06
%% Description:
%%   Tests the Element Valid VC (clause 1), using one of the predefined
%%   internal entities inside an EMPTY content model.
el06(Config) -> run_test(Config, "sun", "invalid/el06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id01
%% Entities: parameter
%% Description:
%%   Tests the ID (is a Name) VC
id01(Config) -> run_test(Config, "sun", "invalid/id01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id02
%% Entities: parameter
%% Description:
%%   Tests the ID (appears once) VC
id02(Config) -> run_test(Config, "sun", "invalid/id02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id03
%% Entities: parameter
%% Description:
%%   Tests the One ID per Element Type VC
id03(Config) -> run_test(Config, "sun", "invalid/id03.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id04
%% Description:
%%   Tests the ID Attribute Default VC
id04(Config) -> run_test(Config, "sun", "invalid/id04.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id05
%% Description:
%%   Tests the ID Attribute Default VC
id05(Config) -> run_test(Config, "sun", "invalid/id05.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id06
%% Description:
%%   Tests the IDREF (is a Name) VC
id06(Config) -> run_test(Config, "sun", "invalid/id06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id07
%% Description:
%%   Tests the IDREFS (is a Names) VC
id07(Config) -> run_test(Config, "sun", "invalid/id07.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id08
%% Description:
%%   Tests the IDREF (matches an ID) VC
id08(Config) -> run_test(Config, "sun", "invalid/id08.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: id09
%% Description:
%%   Tests the IDREF (IDREFS matches an ID) VC
id09(Config) -> run_test(Config, "sun", "invalid/id09.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa01
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that optional
%%   whitespace causes a validity error.
'inv-not-sa01'(Config) -> run_test(Config, "sun", "invalid/not-sa01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa02
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that
%%   attributes needing normalization cause a validity error.
'inv-not-sa02'(Config) -> run_test(Config, "sun", "invalid/not-sa02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa04
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that
%%   attributes needing defaulting cause a validity error.
'inv-not-sa04'(Config) -> run_test(Config, "sun", "invalid/not-sa04.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa05
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that a token
%%   attribute that needs normalization causes a validity error.
'inv-not-sa05'(Config) -> run_test(Config, "sun", "invalid/not-sa05.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa06
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that a
%%   NOTATION attribute that needs normalization causes a validity error.
'inv-not-sa06'(Config) -> run_test(Config, "sun", "invalid/not-sa06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa07
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an
%%   NMTOKEN attribute needing normalization causes a validity error.
'inv-not-sa07'(Config) -> run_test(Config, "sun", "invalid/not-sa07.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa08
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an
%%   NMTOKENS attribute needing normalization causes a validity error.
'inv-not-sa08'(Config) -> run_test(Config, "sun", "invalid/not-sa08.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa09
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an ID
%%   attribute needing normalization causes a validity error.
'inv-not-sa09'(Config) -> run_test(Config, "sun", "invalid/not-sa09.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa10
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an IDREF
%%   attribute needing normalization causes a validity error.
'inv-not-sa10'(Config) -> run_test(Config, "sun", "invalid/not-sa10.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa11
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an
%%   IDREFS attribute needing normalization causes a validity error.
'inv-not-sa11'(Config) -> run_test(Config, "sun", "invalid/not-sa11.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa12
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an
%%   ENTITY attribute needing normalization causes a validity error.
'inv-not-sa12'(Config) -> run_test(Config, "sun", "invalid/not-sa12.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa13
%% Entities: parameter
%% Description:
%%   Tests the Standalone Document Declaration VC, ensuring that an
%%   ENTITIES attribute needing normalization causes a validity error.
'inv-not-sa13'(Config) -> run_test(Config, "sun", "invalid/not-sa13.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-not-sa14
%% Entities: parameter
%% Description:
%%   CDATA sections containing only whitespace do not match the
%%   nonterminal S, and cannot appear in these positions.
'inv-not-sa14'(Config) -> run_test(Config, "sun", "invalid/not-sa14.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional01
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one is required.
optional01(Config) -> run_test(Config, "sun", "invalid/optional01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional02
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing two children where one is required.
optional02(Config) -> run_test(Config, "sun", "invalid/optional02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional03
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where two are required.
optional03(Config) -> run_test(Config, "sun", "invalid/optional03.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional04
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where two are required.
optional04(Config) -> run_test(Config, "sun", "invalid/optional04.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional05
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or two are required
%%   (one construction of that model).
optional05(Config) -> run_test(Config, "sun", "invalid/optional05.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional06
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or two are required
%%   (a second construction of that model).
optional06(Config) -> run_test(Config, "sun", "invalid/optional06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional07
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or two are required
%%   (a third construction of that model).
optional07(Config) -> run_test(Config, "sun", "invalid/optional07.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional08
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or two are required
%%   (a fourth construction of that model).
optional08(Config) -> run_test(Config, "sun", "invalid/optional08.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional09
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or two are required
%%   (a fifth construction of that model).
optional09(Config) -> run_test(Config, "sun", "invalid/optional09.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional10
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where one or two are
%%   required (a basic construction of that model).
optional10(Config) -> run_test(Config, "sun", "invalid/optional10.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional11
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where one or two are
%%   required (a second construction of that model).
optional11(Config) -> run_test(Config, "sun", "invalid/optional11.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional12
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where one or two are
%%   required (a third construction of that model).
optional12(Config) -> run_test(Config, "sun", "invalid/optional12.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional13
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where one or two are
%%   required (a fourth construction of that model).
optional13(Config) -> run_test(Config, "sun", "invalid/optional13.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional14
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing three children where one or two are
%%   required (a fifth construction of that model).
optional14(Config) -> run_test(Config, "sun", "invalid/optional14.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional20
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or more are required
%%   (a sixth construction of that model).
optional20(Config) -> run_test(Config, "sun", "invalid/optional20.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional21
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or more are required
%%   (a seventh construction of that model).
optional21(Config) -> run_test(Config, "sun", "invalid/optional21.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional22
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or more are required
%%   (an eigth construction of that model).
optional22(Config) -> run_test(Config, "sun", "invalid/optional22.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional23
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or more are required
%%   (a ninth construction of that model).
optional23(Config) -> run_test(Config, "sun", "invalid/optional23.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional24
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing no children where one or more are required
%%   (a tenth construction of that model).
optional24(Config) -> run_test(Config, "sun", "invalid/optional24.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: optional25
%% Entities: parameter
%% Description:
%%   Tests the Element Valid VC (clause 2) for one instance of "children"
%%   content model, providing text content where one or more elements are
%%   required.
optional25(Config) -> run_test(Config, "sun", "invalid/optional25.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-required00
%% Description:
%%   Tests the Required Attribute VC.
'inv-required00'(Config) -> run_test(Config, "sun", "invalid/required00.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-required01
%% Description:
%%   Tests the Attribute Value Type (declared) VC for the xml:space
%%   attribute
'inv-required01'(Config) -> run_test(Config, "sun", "invalid/required01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: inv-required02
%% Description:
%%   Tests the Attribute Value Type (declared) VC for the xml:lang
%%   attribute
'inv-required02'(Config) -> run_test(Config, "sun", "invalid/required02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: root
%% Entities: parameter
%% Description:
%%   Tests the Root Element Type VC
root(Config) -> run_test(Config, "sun", "invalid/root.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr01
%% Description:
%%   Tests the "Entity Name" VC for the ENTITY attribute type.
attr01(Config) -> run_test(Config, "sun", "invalid/attr01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr02
%% Description:
%%   Tests the "Entity Name" VC for the ENTITIES attribute type.
attr02(Config) -> run_test(Config, "sun", "invalid/attr02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr03
%% Description:
%%   Tests the "Notation Attributes" VC for the NOTATION attribute type,
%%   first clause: value must be one of the ones that's declared.
attr03(Config) -> run_test(Config, "sun", "invalid/attr03.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr04
%% Description:
%%   Tests the "Notation Attributes" VC for the NOTATION attribute type,
%%   second clause: the names in the declaration must all be declared.
attr04(Config) -> run_test(Config, "sun", "invalid/attr04.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr05
%% Description:
%%   Tests the "Name Token" VC for the NMTOKEN attribute type.
attr05(Config) -> run_test(Config, "sun", "invalid/attr05.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr06
%% Description:
%%   Tests the "Name Token" VC for the NMTOKENS attribute type.
attr06(Config) -> run_test(Config, "sun", "invalid/attr06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr07
%% Description:
%%   Tests the "Enumeration" VC by providing a value which wasn't one of
%%   the choices.
attr07(Config) -> run_test(Config, "sun", "invalid/attr07.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr08
%% Description:
%%   Tests the "Fixed Attribute Default" VC by providing the wrong value.
attr08(Config) -> run_test(Config, "sun", "invalid/attr08.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr09
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal IDREF
%%   value.
attr09(Config) -> run_test(Config, "sun", "invalid/attr09.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr10
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   IDREFS value.
attr10(Config) -> run_test(Config, "sun", "invalid/attr10.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr11
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   ENTITY value.
attr11(Config) -> run_test(Config, "sun", "invalid/attr11.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr12
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   ENTITIES value.
attr12(Config) -> run_test(Config, "sun", "invalid/attr12.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr13
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   NMTOKEN value.
attr13(Config) -> run_test(Config, "sun", "invalid/attr13.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr14
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   NMTOKENS value.
attr14(Config) -> run_test(Config, "sun", "invalid/attr14.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr15
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   NOTATIONS value.
attr15(Config) -> run_test(Config, "sun", "invalid/attr15.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: attr16
%% Description:
%%   Tests the "Attribute Default Legal" VC by providing an illegal
%%   enumeration value.
attr16(Config) -> run_test(Config, "sun", "invalid/attr16.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: utf16b
%% Description:
%%   Tests reading an invalid "big endian" UTF-16 document
utf16b(Config) -> run_test(Config, "sun", "invalid/utf16b.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: utf16l
%% Description:
%%   Tests reading an invalid "little endian" UTF-16 document
utf16l(Config) -> run_test(Config, "sun", "invalid/utf16l.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: empty
%% Description:
%%   CDATA section containing only white space does not match the
%%   nonterminal S, and cannot appear in these positions.
empty(Config) -> run_test(Config, "sun", "invalid/empty.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: not-wf-sa03
%% Entities: parameter
%% Description:
%%   Tests the Entity Declared WFC, ensuring that a reference to
%%   externally defined entity causes a well-formedness error.

%% run_test(Config, "sun", "not-wf/not-sa03.xml", "not-wf").
'not-wf-sa03'(_Config) -> {skip, "ISSUE: Violates standalone=yes"}.

%%----------------------------------------------------------------------
%% ID: attlist01
%% Description:
%%   SGML's NUTOKEN is not allowed.
attlist01(Config) -> run_test(Config, "sun", "not-wf/attlist01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist02
%% Description:
%%   SGML's NUTOKENS attribute type is not allowed.
attlist02(Config) -> run_test(Config, "sun", "not-wf/attlist02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist03
%% Description:
%%   Comma doesn't separate enumerations, unlike in SGML.
attlist03(Config) -> run_test(Config, "sun", "not-wf/attlist03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist04
%% Description:
%%   SGML's NUMBER attribute type is not allowed.
attlist04(Config) -> run_test(Config, "sun", "not-wf/attlist04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist05
%% Description:
%%   SGML's NUMBERS attribute type is not allowed.
attlist05(Config) -> run_test(Config, "sun", "not-wf/attlist05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist06
%% Description:
%%   SGML's NAME attribute type is not allowed.
attlist06(Config) -> run_test(Config, "sun", "not-wf/attlist06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist07
%% Description:
%%   SGML's NAMES attribute type is not allowed.
attlist07(Config) -> run_test(Config, "sun", "not-wf/attlist07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist08
%% Description:
%%   SGML's #CURRENT is not allowed.
attlist08(Config) -> run_test(Config, "sun", "not-wf/attlist08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist09
%% Description:
%%   SGML's #CONREF is not allowed.
attlist09(Config) -> run_test(Config, "sun", "not-wf/attlist09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist10
%% Description:
%%   Whitespace required between attributes
attlist10(Config) -> run_test(Config, "sun", "not-wf/attlist10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: attlist11
%% Description:
%%   Whitespace required between attributes
attlist11(Config) -> run_test(Config, "sun", "not-wf/attlist11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: cond01
%% Entities: parameter
%% Description:
%%   Only INCLUDE and IGNORE are conditional section keywords
cond01(Config) -> run_test(Config, "sun", "not-wf/cond01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: cond02
%% Entities: parameter
%% Description:
%%   Must have keyword in conditional sections
cond02(Config) -> run_test(Config, "sun", "not-wf/cond02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: content01
%% Description:
%%   No whitespace before "?" in content model
content01(Config) -> run_test(Config, "sun", "not-wf/content01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: content02
%% Description:
%%   No whitespace before "*" in content model
content02(Config) -> run_test(Config, "sun", "not-wf/content02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: content03
%% Description:
%%   No whitespace before "+" in content model
content03(Config) -> run_test(Config, "sun", "not-wf/content03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: decl01
%% Entities: parameter
%% Description:
%%   External entities may not have standalone decls.
decl01(Config) -> run_test(Config, "sun", "not-wf/decl01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: nwf-dtd00
%% Description:
%%   Comma mandatory in content model
'nwf-dtd00'(Config) -> run_test(Config, "sun", "not-wf/dtd00.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: nwf-dtd01
%% Description:
%%   Can't mix comma and vertical bar in content models
'nwf-dtd01'(Config) -> run_test(Config, "sun", "not-wf/dtd01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: dtd02
%% Description:
%%   PE name immediately after "%"
dtd02(Config) -> run_test(Config, "sun", "not-wf/dtd02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: dtd03
%% Description:
%%   PE name immediately followed by ";"
dtd03(Config) -> run_test(Config, "sun", "not-wf/dtd03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: dtd04
%% Description:
%%   PUBLIC literal must be quoted
dtd04(Config) -> run_test(Config, "sun", "not-wf/dtd04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: dtd05
%% Description:
%%   SYSTEM identifier must be quoted
dtd05(Config) -> run_test(Config, "sun", "not-wf/dtd05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: dtd07
%% Entities: parameter
%% Description:
%%   Text declarations (which optionally begin any external entity) are
%%   required to have "encoding=...".
dtd07(Config) -> run_test(Config, "sun", "not-wf/dtd07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: element00
%% Description:
%%   EOF in middle of incomplete ETAG
element00(Config) -> run_test(Config, "sun", "not-wf/element00.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: element01
%% Description:
%%   EOF in middle of incomplete ETAG
element01(Config) -> run_test(Config, "sun", "not-wf/element01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: element02
%% Description:
%%   Illegal markup (<%@ ... %>)
element02(Config) -> run_test(Config, "sun", "not-wf/element02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: element03
%% Description:
%%   Illegal markup (<% ... %>)
element03(Config) -> run_test(Config, "sun", "not-wf/element03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: element04
%% Description:
%%   Illegal markup (<!ELEMENT ... >)
element04(Config) -> run_test(Config, "sun", "not-wf/element04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding01
%% Description:
%%   Illegal character " " in encoding name
encoding01(Config) -> run_test(Config, "sun", "not-wf/encoding01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding02
%% Description:
%%   Illegal character "/" in encoding name
encoding02(Config) -> run_test(Config, "sun", "not-wf/encoding02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding03
%% Description:
%%   Illegal character reference in encoding name
encoding03(Config) -> run_test(Config, "sun", "not-wf/encoding03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding04
%% Description:
%%   Illegal character ":" in encoding name
encoding04(Config) -> run_test(Config, "sun", "not-wf/encoding04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding05
%% Description:
%%   Illegal character "@" in encoding name
encoding05(Config) -> run_test(Config, "sun", "not-wf/encoding05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding06
%% Description:
%%   Illegal character "+" in encoding name
encoding06(Config) -> run_test(Config, "sun", "not-wf/encoding06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: encoding07
%% Entities: general
%% Description:
%%   Text declarations (which optionally begin any external entity) are
%%   required to have "encoding=...".
encoding07(Config) -> run_test(Config, "sun", "not-wf/encoding07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pi
%% Description:
%%   No space between PI target name and data
pi(Config) -> run_test(Config, "sun", "not-wf/pi.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pubid01
%% Description:
%%   Illegal entity ref in public ID
pubid01(Config) -> run_test(Config, "sun", "not-wf/pubid01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pubid02
%% Description:
%%   Illegal characters in public ID
pubid02(Config) -> run_test(Config, "sun", "not-wf/pubid02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pubid03
%% Description:
%%   Illegal characters in public ID
pubid03(Config) -> run_test(Config, "sun", "not-wf/pubid03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pubid04
%% Description:
%%   Illegal characters in public ID
pubid04(Config) -> run_test(Config, "sun", "not-wf/pubid04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: pubid05
%% Description:
%%   SGML-ism: public ID without system ID
pubid05(Config) -> run_test(Config, "sun", "not-wf/pubid05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml01
%% Description:
%%   SGML-ism: omitted end tag for EMPTY content
sgml01(Config) -> run_test(Config, "sun", "not-wf/sgml01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml02
%% Description:
%%   XML declaration must be at the very beginning of a document; it"s
%%   not a processing instruction
sgml02(Config) -> run_test(Config, "sun", "not-wf/sgml02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml03
%% Description:
%%   Comments may not contain "--"
sgml03(Config) -> run_test(Config, "sun", "not-wf/sgml03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml04
%% Description:
%%   ATTLIST declarations apply to only one element, unlike SGML
sgml04(Config) -> run_test(Config, "sun", "not-wf/sgml04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml05
%% Description:
%%   ELEMENT declarations apply to only one element, unlike SGML
sgml05(Config) -> run_test(Config, "sun", "not-wf/sgml05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml06
%% Description:
%%   ATTLIST declarations are never global, unlike in SGML
sgml06(Config) -> run_test(Config, "sun", "not-wf/sgml06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml07
%% Description:
%%   SGML Tag minimization specifications are not allowed
sgml07(Config) -> run_test(Config, "sun", "not-wf/sgml07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml08
%% Description:
%%   SGML Tag minimization specifications are not allowed
sgml08(Config) -> run_test(Config, "sun", "not-wf/sgml08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml09
%% Description:
%%   SGML Content model exception specifications are not allowed
sgml09(Config) -> run_test(Config, "sun", "not-wf/sgml09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml10
%% Description:
%%   SGML Content model exception specifications are not allowed
sgml10(Config) -> run_test(Config, "sun", "not-wf/sgml10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml11
%% Description:
%%   CDATA is not a valid content model spec
sgml11(Config) -> run_test(Config, "sun", "not-wf/sgml11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml12
%% Description:
%%   RCDATA is not a valid content model spec
sgml12(Config) -> run_test(Config, "sun", "not-wf/sgml12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: sgml13
%% Description:
%%   SGML Unordered content models not allowed
sgml13(Config) -> run_test(Config, "sun", "not-wf/sgml13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: uri01
%% Description:
%%   SYSTEM ids may not have URI fragments
uri01(Config) -> run_test(Config, "sun", "not-wf/uri01.xml", "error").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: OASIS/NIST XML 1.0 Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: o-p01pass2
%% Description:
%%   various Misc items where they can occur
'o-p01pass2'(Config) -> run_test(Config, "oasis", "p01pass2.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p06pass1
%% Description:
%%   various satisfactions of the Names production in a NAMES attribute
'o-p06pass1'(Config) -> run_test(Config, "oasis", "p06pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p07pass1
%% Description:
%%   various valid Nmtoken 's in an attribute list declaration.
'o-p07pass1'(Config) -> run_test(Config, "oasis", "p07pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p08pass1
%% Description:
%%   various satisfaction of an NMTOKENS attribute value.
'o-p08pass1'(Config) -> run_test(Config, "oasis", "p08pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p09pass1
%% Entities: parameter
%% Description:
%%   valid EntityValue's. Except for entity references, markup is not
%%   recognized.
'o-p09pass1'(Config) -> run_test(Config, "oasis", "p09pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p12pass1
%% Description:
%%   valid public IDs.
'o-p12pass1'(Config) -> run_test(Config, "oasis", "p12pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p22pass4
%% Description:
%%   XML decl and doctypedecl
'o-p22pass4'(Config) -> run_test(Config, "oasis", "p22pass4.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p22pass5
%% Description:
%%   just doctypedecl
'o-p22pass5'(Config) -> run_test(Config, "oasis", "p22pass5.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p22pass6
%% Description:
%%   S between decls is not required
'o-p22pass6'(Config) -> run_test(Config, "oasis", "p22pass6.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p28pass1
%% Description:
%%   Empty-element tag must be used for element which are declared EMPTY.
'o-p28pass1'(Config) -> run_test(Config, "oasis", "p28pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p28pass3
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with Parameter entity reference. The declaration
%%   of a parameter entity must precede any reference to it.
'o-p28pass3'(Config) -> run_test(Config, "oasis", "p28pass3.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p28pass4
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with ExternalID as an External Entity declaration.
'o-p28pass4'(Config) -> run_test(Config, "oasis", "p28pass4.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p28pass5
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with ExternalID as an External Entity. A parameter
%%   entity reference is also used.
'o-p28pass5'(Config) -> run_test(Config, "oasis", "p28pass5.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p29pass1
%% Description:
%%   Valid types of markupdecl.
'o-p29pass1'(Config) -> run_test(Config, "oasis", "p29pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p30pass1
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with ExternalID as an External Entity. The
%%   external entity has an element declaration.
'o-p30pass1'(Config) -> run_test(Config, "oasis", "p30pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p30pass2
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with ExternalID as an Enternal Entity. The
%%   external entity begins with a Text Declaration.
'o-p30pass2'(Config) -> run_test(Config, "oasis", "p30pass2.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p31pass1
%% Entities: parameter
%% Description:
%%   external subset can be empty
'o-p31pass1'(Config) -> run_test(Config, "oasis", "p31pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p31pass2
%% Entities: parameter
%% Description:
%%   Valid doctypedecl with EXternalID as Enternal Entity. The external
%%   entity contains a parameter entity reference and condtional
%%   sections.
'o-p31pass2'(Config) -> run_test(Config, "oasis", "p31pass2.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p43pass1
%% Description:
%%   Valid use of character data, comments, processing instructions and
%%   CDATA sections within the start and end tag.
'o-p43pass1'(Config) -> run_test(Config, "oasis", "p43pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p45pass1
%% Description:
%%   valid element declarations
'o-p45pass1'(Config) -> run_test(Config, "oasis", "p45pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p46pass1
%% Description:
%%   Valid use of contentspec, element content models, and mixed content
%%   within an element type declaration.
'o-p46pass1'(Config) -> run_test(Config, "oasis", "p46pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p47pass1
%% Description:
%%   Valid use of contentspec, element content models, choices, sequences
%%   and content particles within an element type declaration. The
%%   optional character following a name or list governs the number of
%%   times the element or content particle may appear.
'o-p47pass1'(Config) -> run_test(Config, "oasis", "p47pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p48pass1
%% Description:
%%   Valid use of contentspec, element content models, choices, sequences
%%   and content particles within an element type declaration. The
%%   optional character following a name or list governs the number of
%%   times the element or content particle may appear.
'o-p48pass1'(Config) -> run_test(Config, "oasis", "p48pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p49pass1
%% Description:
%%   Valid use of contentspec, element content models, choices, and
%%   content particles within an element type declaration. The optional
%%   character following a name or list governs the number of times the
%%   element or content particle may appear. Whitespace is also valid
%%   between choices.
'o-p49pass1'(Config) -> run_test(Config, "oasis", "p49pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p50pass1
%% Description:
%%   Valid use of contentspec, element content models, sequences and
%%   content particles within an element type declaration. The optional
%%   character following a name or list governs the number of times the
%%   element or content particle may appear. Whitespace is also valid
%%   between sequences.
'o-p50pass1'(Config) -> run_test(Config, "oasis", "p50pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p51pass1
%% Description:
%%   valid Mixed contentspec's.
'o-p51pass1'(Config) -> run_test(Config, "oasis", "p51pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p52pass1
%% Description:
%%   valid AttlistDecls: No AttDef's are required, and the terminating S
%%   is optional, multiple ATTLISTS per element are OK, and multiple
%%   declarations of the same attribute are OK.
'o-p52pass1'(Config) -> run_test(Config, "oasis", "p52pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p53pass1
%% Description:
%%   a valid AttDef
'o-p53pass1'(Config) -> run_test(Config, "oasis", "p53pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p54pass1
%% Description:
%%   the three kinds of attribute types
'o-p54pass1'(Config) -> run_test(Config, "oasis", "p54pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p55pass1
%% Description:
%%   StringType = "CDATA"
'o-p55pass1'(Config) -> run_test(Config, "oasis", "p55pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p56pass1
%% Description:
%%   the 7 tokenized attribute types
'o-p56pass1'(Config) -> run_test(Config, "oasis", "p56pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p57pass1
%% Description:
%%   enumerated types are NMTOKEN or NOTATION lists
'o-p57pass1'(Config) -> run_test(Config, "oasis", "p57pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p58pass1
%% Description:
%%   NOTATION enumeration has on or more items
'o-p58pass1'(Config) -> run_test(Config, "oasis", "p58pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p59pass1
%% Description:
%%   NMTOKEN enumerations haveon or more items
'o-p59pass1'(Config) -> run_test(Config, "oasis", "p59pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p60pass1
%% Description:
%%   the four types of default values
'o-p60pass1'(Config) -> run_test(Config, "oasis", "p60pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p61pass1
%% Entities: parameter
%% Description:
%%   valid conditional sections are INCLUDE and IGNORE
'o-p61pass1'(Config) -> run_test(Config, "oasis", "p61pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p62pass1
%% Entities: parameter
%% Description:
%%   valid INCLUDE sections -- options S before and after keyword,
%%   sections can nest
'o-p62pass1'(Config) -> run_test(Config, "oasis", "p62pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p63pass1
%% Entities: parameter
%% Description:
%%   valid IGNORE sections
'o-p63pass1'(Config) -> run_test(Config, "oasis", "p63pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p64pass1
%% Entities: parameter
%% Description:
%%   IGNOREd sections ignore everything except section delimiters
'o-p64pass1'(Config) -> run_test(Config, "oasis", "p64pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p68pass1
%% Description:
%%   Valid entity references. Also ensures that a charref to '&' isn't
%%   interpreted as an entity reference open delimiter
'o-p68pass1'(Config) -> run_test(Config, "oasis", "p68pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p69pass1
%% Entities: parameter
%% Description:
%%   Valid PEReferences.
'o-p69pass1'(Config) -> run_test(Config, "oasis", "p69pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p70pass1
%% Description:
%%   An EntityDecl is either a GEDecl or a PEDecl
'o-p70pass1'(Config) -> run_test(Config, "oasis", "p70pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p71pass1
%% Description:
%%   Valid GEDecls
'o-p71pass1'(Config) -> run_test(Config, "oasis", "p71pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p72pass1
%% Description:
%%   Valid PEDecls
'o-p72pass1'(Config) -> run_test(Config, "oasis", "p72pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p73pass1
%% Description:
%%   EntityDef is either Entity value or an external id, with an optional
%%   NDataDecl
'o-p73pass1'(Config) -> run_test(Config, "oasis", "p73pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p76pass1
%% Description:
%%   valid NDataDecls
'o-p76pass1'(Config) -> run_test(Config, "oasis", "p76pass1.xml", "valid").

%%----------------------------------------------------------------------
%% ID: o-p01pass1
%% Description:
%%   no prolog
'o-p01pass1'(Config) -> run_test(Config, "oasis", "p01pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p01pass3
%% Description:
%%   Misc items after the document
'o-p01pass3'(Config) -> run_test(Config, "oasis", "p01pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p03pass1
%% Description:
%%   all valid S characters
'o-p03pass1'(Config) -> run_test(Config, "oasis", "p03pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p04pass1
%% Description:
%%   names with all valid ASCII characters, and one from each other class
%%   in NameChar

%% run_test(Config, "oasis", "p04pass1.xml", "invalid").
'o-p04pass1'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: o-p05pass1
%% Description:
%%   various valid Name constructions

%% run_test(Config, "oasis", "p05pass1.xml", "invalid").
'o-p05pass1'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: o-p06fail1
%% Description:
%%   Requires at least one name.
'o-p06fail1'(Config) -> run_test(Config, "oasis", "p06fail1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p08fail1
%% Description:
%%   at least one Nmtoken is required.
'o-p08fail1'(Config) -> run_test(Config, "oasis", "p08fail1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p08fail2
%% Description:
%%   an invalid Nmtoken character.
'o-p08fail2'(Config) -> run_test(Config, "oasis", "p08fail2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p10pass1
%% Description:
%%   valid attribute values
'o-p10pass1'(Config) -> run_test(Config, "oasis", "p10pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p14pass1
%% Description:
%%   valid CharData
'o-p14pass1'(Config) -> run_test(Config, "oasis", "p14pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p15pass1
%% Description:
%%   valid comments
'o-p15pass1'(Config) -> run_test(Config, "oasis", "p15pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p16pass1
%% Description:
%%   Valid form of Processing Instruction. Shows that whitespace
%%   character data is valid before end of processing instruction.
'o-p16pass1'(Config) -> run_test(Config, "oasis", "p16pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p16pass2
%% Description:
%%   Valid form of Processing Instruction. Shows that whitespace
%%   character data is valid before end of processing instruction.
'o-p16pass2'(Config) -> run_test(Config, "oasis", "p16pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p16pass3
%% Description:
%%   Valid form of Processing Instruction. Shows that whitespace
%%   character data is valid before end of processing instruction.
'o-p16pass3'(Config) -> run_test(Config, "oasis", "p16pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p18pass1
%% Description:
%%   valid CDSect's. Note that a CDStart in a CDSect is not recognized as
%%   such
'o-p18pass1'(Config) -> run_test(Config, "oasis", "p18pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p22pass1
%% Description:
%%   prolog can be empty
'o-p22pass1'(Config) -> run_test(Config, "oasis", "p22pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p22pass2
%% Description:
%%   XML declaration only
'o-p22pass2'(Config) -> run_test(Config, "oasis", "p22pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p22pass3
%% Description:
%%   XML decl and Misc
'o-p22pass3'(Config) -> run_test(Config, "oasis", "p22pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p23pass1
%% Description:
%%   Test shows a valid XML declaration along with version info.
'o-p23pass1'(Config) -> run_test(Config, "oasis", "p23pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p23pass2
%% Description:
%%   Test shows a valid XML declaration along with encoding declaration.
'o-p23pass2'(Config) -> run_test(Config, "oasis", "p23pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p23pass3
%% Description:
%%   Test shows a valid XML declaration along with Standalone Document
%%   Declaration.
'o-p23pass3'(Config) -> run_test(Config, "oasis", "p23pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p23pass4
%% Description:
%%   Test shows a valid XML declaration, encoding declarationand
%%   Standalone Document Declaration.
'o-p23pass4'(Config) -> run_test(Config, "oasis", "p23pass4.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p24pass1
%% Description:
%%   Test shows a prolog that has the VersionInfo delimited by double
%%   quotes.
'o-p24pass1'(Config) -> run_test(Config, "oasis", "p24pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p24pass2
%% Description:
%%   Test shows a prolog that has the VersionInfo delimited by single
%%   quotes.
'o-p24pass2'(Config) -> run_test(Config, "oasis", "p24pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p24pass3
%% Description:
%%   Test shows whitespace is allowed in prolog before version info.
'o-p24pass3'(Config) -> run_test(Config, "oasis", "p24pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p24pass4
%% Description:
%%   Test shows whitespace is allowed in prolog on both sides of equal
%%   sign.
'o-p24pass4'(Config) -> run_test(Config, "oasis", "p24pass4.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p25pass1
%% Description:
%%   Test shows whitespace is NOT necessary before or after equal sign of
%%   versioninfo.
'o-p25pass1'(Config) -> run_test(Config, "oasis", "p25pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p25pass2
%% Description:
%%   Test shows whitespace can be used on both sides of equal sign of
%%   versioninfo.
'o-p25pass2'(Config) -> run_test(Config, "oasis", "p25pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p26pass1
%% Description:
%%   The valid version number. We cannot test others because a 1.0
%%   processor is allowed to fail them.
'o-p26pass1'(Config) -> run_test(Config, "oasis", "p26pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p27pass1
%% Description:
%%   Comments are valid as the Misc part of the prolog.
'o-p27pass1'(Config) -> run_test(Config, "oasis", "p27pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p27pass2
%% Description:
%%   Processing Instructions are valid as the Misc part of the prolog.
'o-p27pass2'(Config) -> run_test(Config, "oasis", "p27pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p27pass3
%% Description:
%%   Whitespace is valid as the Misc part of the prolog.
'o-p27pass3'(Config) -> run_test(Config, "oasis", "p27pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p27pass4
%% Description:
%%   A combination of comments, whitespaces and processing instructions
%%   are valid as the Misc part of the prolog.
'o-p27pass4'(Config) -> run_test(Config, "oasis", "p27pass4.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p32pass1
%% Description:
%%   Double quotes can be used as delimeters for the value of a
%%   Standalone Document Declaration.
'o-p32pass1'(Config) -> run_test(Config, "oasis", "p32pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p32pass2
%% Description:
%%   Single quotes can be used as delimeters for the value of a
%%   Standalone Document Declaration.
'o-p32pass2'(Config) -> run_test(Config, "oasis", "p32pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p39pass1
%% Description:
%%   Empty element tag may be used for any element which has no content.
'o-p39pass1'(Config) -> run_test(Config, "oasis", "p39pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p39pass2
%% Description:
%%   Character data is valid element content.
'o-p39pass2'(Config) -> run_test(Config, "oasis", "p39pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p40pass1
%% Description:
%%   Elements content can be empty.
'o-p40pass1'(Config) -> run_test(Config, "oasis", "p40pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p40pass2
%% Description:
%%   Whitespace is valid within a Start-tag.
'o-p40pass2'(Config) -> run_test(Config, "oasis", "p40pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p40pass3
%% Description:
%%   Attributes are valid within a Start-tag.
'o-p40pass3'(Config) -> run_test(Config, "oasis", "p40pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p40pass4
%% Description:
%%   Whitespace and Multiple Attributes are valid within a Start-tag.
'o-p40pass4'(Config) -> run_test(Config, "oasis", "p40pass4.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p41pass1
%% Description:
%%   Attributes are valid within a Start-tag.
'o-p41pass1'(Config) -> run_test(Config, "oasis", "p41pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p41pass2
%% Description:
%%   Whitespace is valid within a Start-tags Attribute.
'o-p41pass2'(Config) -> run_test(Config, "oasis", "p41pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p42pass1
%% Description:
%%   Test shows proper syntax for an End-tag.
'o-p42pass1'(Config) -> run_test(Config, "oasis", "p42pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p42pass2
%% Description:
%%   Whitespace is valid after name in End-tag.
'o-p42pass2'(Config) -> run_test(Config, "oasis", "p42pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p44pass1
%% Description:
%%   Valid display of an Empty Element Tag.
'o-p44pass1'(Config) -> run_test(Config, "oasis", "p44pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p44pass2
%% Description:
%%   Empty Element Tags can contain an Attribute.
'o-p44pass2'(Config) -> run_test(Config, "oasis", "p44pass2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p44pass3
%% Description:
%%   Whitespace is valid in an Empty Element Tag following the end of the
%%   attribute value.
'o-p44pass3'(Config) -> run_test(Config, "oasis", "p44pass3.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p44pass4
%% Description:
%%   Whitespace is valid after the name in an Empty Element Tag.
'o-p44pass4'(Config) -> run_test(Config, "oasis", "p44pass4.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p44pass5
%% Description:
%%   Whitespace and Multiple Attributes are valid in an Empty Element
%%   Tag.
'o-p44pass5'(Config) -> run_test(Config, "oasis", "p44pass5.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p66pass1
%% Description:
%%   valid character references
'o-p66pass1'(Config) -> run_test(Config, "oasis", "p66pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p74pass1
%% Description:
%%   PEDef is either an entity value or an external id
'o-p74pass1'(Config) -> run_test(Config, "oasis", "p74pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p75pass1
%% Description:
%%   valid external identifiers
'o-p75pass1'(Config) -> run_test(Config, "oasis", "p75pass1.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-e2
%% Description:
%%   Validity Constraint: No duplicate tokens
'o-e2'(Config) -> run_test(Config, "oasis", "e2.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: o-p01fail1
%% Description:
%%   S cannot occur before the prolog
'o-p01fail1'(Config) -> run_test(Config, "oasis", "p01fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p01fail2
%% Description:
%%   comments cannot occur before the prolog
'o-p01fail2'(Config) -> run_test(Config, "oasis", "p01fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p01fail3
%% Description:
%%   only one document element
'o-p01fail3'(Config) -> run_test(Config, "oasis", "p01fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p01fail4
%% Description:
%%   document element must be complete.
'o-p01fail4'(Config) -> run_test(Config, "oasis", "p01fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail1
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail1'(Config) -> run_test(Config, "oasis", "p02fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail10
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail10'(Config) -> run_test(Config, "oasis", "p02fail10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail11
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail11'(Config) -> run_test(Config, "oasis", "p02fail11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail12
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail12'(Config) -> run_test(Config, "oasis", "p02fail12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail13
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail13'(Config) -> run_test(Config, "oasis", "p02fail13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail14
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail14'(Config) -> run_test(Config, "oasis", "p02fail14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail15
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail15'(Config) -> run_test(Config, "oasis", "p02fail15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail16
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail16'(Config) -> run_test(Config, "oasis", "p02fail16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail17
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail17'(Config) -> run_test(Config, "oasis", "p02fail17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail18
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail18'(Config) -> run_test(Config, "oasis", "p02fail18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail19
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail19'(Config) -> run_test(Config, "oasis", "p02fail19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail2
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail2'(Config) -> run_test(Config, "oasis", "p02fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail20
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail20'(Config) -> run_test(Config, "oasis", "p02fail20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail21
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail21'(Config) -> run_test(Config, "oasis", "p02fail21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail22
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail22'(Config) -> run_test(Config, "oasis", "p02fail22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail23
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail23'(Config) -> run_test(Config, "oasis", "p02fail23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail24
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail24'(Config) -> run_test(Config, "oasis", "p02fail24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail25
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail25'(Config) -> run_test(Config, "oasis", "p02fail25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail26
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail26'(Config) -> run_test(Config, "oasis", "p02fail26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail27
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail27'(Config) -> run_test(Config, "oasis", "p02fail27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail28
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail28'(Config) -> run_test(Config, "oasis", "p02fail28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail29
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail29'(Config) -> run_test(Config, "oasis", "p02fail29.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail3
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail3'(Config) -> run_test(Config, "oasis", "p02fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail30
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail30'(Config) -> run_test(Config, "oasis", "p02fail30.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail31
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail31'(Config) -> run_test(Config, "oasis", "p02fail31.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail4
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail4'(Config) -> run_test(Config, "oasis", "p02fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail5
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail5'(Config) -> run_test(Config, "oasis", "p02fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail6
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail6'(Config) -> run_test(Config, "oasis", "p02fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail7
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail7'(Config) -> run_test(Config, "oasis", "p02fail7.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail8
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail8'(Config) -> run_test(Config, "oasis", "p02fail8.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p02fail9
%% Description:
%%   Use of illegal character within XML document.
'o-p02fail9'(Config) -> run_test(Config, "oasis", "p02fail9.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail1
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail1'(Config) -> run_test(Config, "oasis", "p03fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail10
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail10'(Config) -> run_test(Config, "oasis", "p03fail10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail11
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail11'(Config) -> run_test(Config, "oasis", "p03fail11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail12
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail12'(Config) -> run_test(Config, "oasis", "p03fail12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail13
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail13'(Config) -> run_test(Config, "oasis", "p03fail13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail14
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail14'(Config) -> run_test(Config, "oasis", "p03fail14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail15
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail15'(Config) -> run_test(Config, "oasis", "p03fail15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail16
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail16'(Config) -> run_test(Config, "oasis", "p03fail16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail17
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail17'(Config) -> run_test(Config, "oasis", "p03fail17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail18
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail18'(Config) -> run_test(Config, "oasis", "p03fail18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail19
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail19'(Config) -> run_test(Config, "oasis", "p03fail19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail2
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail2'(Config) -> run_test(Config, "oasis", "p03fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail20
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail20'(Config) -> run_test(Config, "oasis", "p03fail20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail21
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail21'(Config) -> run_test(Config, "oasis", "p03fail21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail22
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail22'(Config) -> run_test(Config, "oasis", "p03fail22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail23
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail23'(Config) -> run_test(Config, "oasis", "p03fail23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail24
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail24'(Config) -> run_test(Config, "oasis", "p03fail24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail25
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail25'(Config) -> run_test(Config, "oasis", "p03fail25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail26
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail26'(Config) -> run_test(Config, "oasis", "p03fail26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail27
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail27'(Config) -> run_test(Config, "oasis", "p03fail27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail28
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail28'(Config) -> run_test(Config, "oasis", "p03fail28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail29
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail29'(Config) -> run_test(Config, "oasis", "p03fail29.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail3
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail3'(Config) -> run_test(Config, "oasis", "p03fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail4
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail4'(Config) -> run_test(Config, "oasis", "p03fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail5
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail5'(Config) -> run_test(Config, "oasis", "p03fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail7
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail7'(Config) -> run_test(Config, "oasis", "p03fail7.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail8
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail8'(Config) -> run_test(Config, "oasis", "p03fail8.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p03fail9
%% Description:
%%   Use of illegal character within XML document.
'o-p03fail9'(Config) -> run_test(Config, "oasis", "p03fail9.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p04fail1
%% Description:
%%   Name contains invalid character.
'o-p04fail1'(Config) -> run_test(Config, "oasis", "p04fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p04fail2
%% Description:
%%   Name contains invalid character.
'o-p04fail2'(Config) -> run_test(Config, "oasis", "p04fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p04fail3
%% Description:
%%   Name contains invalid character.
'o-p04fail3'(Config) -> run_test(Config, "oasis", "p04fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p05fail1
%% Description:
%%   a Name cannot start with a digit
'o-p05fail1'(Config) -> run_test(Config, "oasis", "p05fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p05fail2
%% Description:
%%   a Name cannot start with a '.'
'o-p05fail2'(Config) -> run_test(Config, "oasis", "p05fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p05fail3
%% Description:
%%   a Name cannot start with a "-"
'o-p05fail3'(Config) -> run_test(Config, "oasis", "p05fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p05fail4
%% Description:
%%   a Name cannot start with a CombiningChar
'o-p05fail4'(Config) -> run_test(Config, "oasis", "p05fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p05fail5
%% Description:
%%   a Name cannot start with an Extender
'o-p05fail5'(Config) -> run_test(Config, "oasis", "p05fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p09fail1
%% Entities: parameter
%% Description:
%%   EntityValue excludes '%'
'o-p09fail1'(Config) -> run_test(Config, "oasis", "p09fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p09fail2
%% Entities: parameter
%% Description:
%%   EntityValue excludes '&'
'o-p09fail2'(Config) -> run_test(Config, "oasis", "p09fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p09fail3
%% Description:
%%   incomplete character reference
'o-p09fail3'(Config) -> run_test(Config, "oasis", "p09fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p09fail4
%% Description:
%%   quote types must match
'o-p09fail4'(Config) -> run_test(Config, "oasis", "p09fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p09fail5
%% Description:
%%   quote types must match
'o-p09fail5'(Config) -> run_test(Config, "oasis", "p09fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p10fail1
%% Description:
%%   attribute values exclude '<'
'o-p10fail1'(Config) -> run_test(Config, "oasis", "p10fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p10fail2
%% Description:
%%   attribute values exclude '&'
'o-p10fail2'(Config) -> run_test(Config, "oasis", "p10fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p10fail3
%% Description:
%%   quote types must match
'o-p10fail3'(Config) -> run_test(Config, "oasis", "p10fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p11fail1
%% Description:
%%   quote types must match
'o-p11fail1'(Config) -> run_test(Config, "oasis", "p11fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p11fail2
%% Description:
%%   cannot contain delimiting quotes
'o-p11fail2'(Config) -> run_test(Config, "oasis", "p11fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail1
%% Description:
%%   '"' excluded
'o-p12fail1'(Config) -> run_test(Config, "oasis", "p12fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail2
%% Description:
%%   '\' excluded
'o-p12fail2'(Config) -> run_test(Config, "oasis", "p12fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail3
%% Description:
%%   entity references excluded
'o-p12fail3'(Config) -> run_test(Config, "oasis", "p12fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail4
%% Description:
%%   '>' excluded
'o-p12fail4'(Config) -> run_test(Config, "oasis", "p12fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail5
%% Description:
%%   '<' excluded
'o-p12fail5'(Config) -> run_test(Config, "oasis", "p12fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail6
%% Description:
%%   built-in entity refs excluded
'o-p12fail6'(Config) -> run_test(Config, "oasis", "p12fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p12fail7
%% Description:
%%   The public ID has a tab character, which is disallowed
'o-p12fail7'(Config) -> run_test(Config, "oasis", "p12fail7.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p14fail1
%% Description:
%%   '<' excluded
'o-p14fail1'(Config) -> run_test(Config, "oasis", "p14fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p14fail2
%% Description:
%%   '&' excluded
'o-p14fail2'(Config) -> run_test(Config, "oasis", "p14fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p14fail3
%% Description:
%%   "]]>" excluded
'o-p14fail3'(Config) -> run_test(Config, "oasis", "p14fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p15fail1
%% Description:
%%   comments can't end in '-'
'o-p15fail1'(Config) -> run_test(Config, "oasis", "p15fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p15fail2
%% Description:
%%   one comment per comment (contrasted with SGML)
'o-p15fail2'(Config) -> run_test(Config, "oasis", "p15fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p15fail3
%% Description:
%%   can't include 2 or more adjacent '-'s
'o-p15fail3'(Config) -> run_test(Config, "oasis", "p15fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p16fail1
%% Description:
%%   "xml" is an invalid PITarget
'o-p16fail1'(Config) -> run_test(Config, "oasis", "p16fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p16fail2
%% Description:
%%   a PITarget must be present
'o-p16fail2'(Config) -> run_test(Config, "oasis", "p16fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p16fail3
%% Description:
%%   S after PITarget is required
'o-p16fail3'(Config) -> run_test(Config, "oasis", "p16fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p18fail1
%% Description:
%%   no space before "CDATA"
'o-p18fail1'(Config) -> run_test(Config, "oasis", "p18fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p18fail2
%% Description:
%%   no space after "CDATA"
'o-p18fail2'(Config) -> run_test(Config, "oasis", "p18fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p18fail3
%% Description:
%%   CDSect's can't nest
'o-p18fail3'(Config) -> run_test(Config, "oasis", "p18fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p22fail1
%% Description:
%%   prolog must start with XML decl
'o-p22fail1'(Config) -> run_test(Config, "oasis", "p22fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p22fail2
%% Description:
%%   prolog must start with XML decl
'o-p22fail2'(Config) -> run_test(Config, "oasis", "p22fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p23fail1
%% Description:
%%   "xml" must be lower-case
'o-p23fail1'(Config) -> run_test(Config, "oasis", "p23fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p23fail2
%% Description:
%%   VersionInfo must be supplied
'o-p23fail2'(Config) -> run_test(Config, "oasis", "p23fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p23fail3
%% Description:
%%   VersionInfo must come first
'o-p23fail3'(Config) -> run_test(Config, "oasis", "p23fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p23fail4
%% Description:
%%   SDDecl must come last
'o-p23fail4'(Config) -> run_test(Config, "oasis", "p23fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p23fail5
%% Description:
%%   no SGML-type PIs
'o-p23fail5'(Config) -> run_test(Config, "oasis", "p23fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p24fail1
%% Description:
%%   quote types must match
'o-p24fail1'(Config) -> run_test(Config, "oasis", "p24fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p24fail2
%% Description:
%%   quote types must match
'o-p24fail2'(Config) -> run_test(Config, "oasis", "p24fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p25fail1
%% Description:
%%   Comment is illegal in VersionInfo.
'o-p25fail1'(Config) -> run_test(Config, "oasis", "p25fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p26fail1
%% Description:
%%   Illegal character in VersionNum.
'o-p26fail1'(Config) -> run_test(Config, "oasis", "p26fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p26fail2
%% Description:
%%   Illegal character in VersionNum.
'o-p26fail2'(Config) -> run_test(Config, "oasis", "p26fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p27fail1
%% Description:
%%   References aren't allowed in Misc, even if they would resolve to
%%   valid Misc.
'o-p27fail1'(Config) -> run_test(Config, "oasis", "p27fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p28fail1
%% Description:
%%   only declarations in DTD.
'o-p28fail1'(Config) -> run_test(Config, "oasis", "p28fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p29fail1
%% Description:
%%   A processor must not pass unknown declaration types.
'o-p29fail1'(Config) -> run_test(Config, "oasis", "p29fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p30fail1
%% Entities: parameter
%% Description:
%%   An XML declaration is not the same as a TextDecl
'o-p30fail1'(Config) -> run_test(Config, "oasis", "p30fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p31fail1
%% Entities: parameter
%% Description:
%%   external subset excludes doctypedecl
'o-p31fail1'(Config) -> run_test(Config, "oasis", "p31fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p32fail1
%% Description:
%%   quote types must match
'o-p32fail1'(Config) -> run_test(Config, "oasis", "p32fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p32fail2
%% Description:
%%   quote types must match
'o-p32fail2'(Config) -> run_test(Config, "oasis", "p32fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p32fail3
%% Description:
%%   initial S is required
'o-p32fail3'(Config) -> run_test(Config, "oasis", "p32fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p32fail4
%% Description:
%%   quotes are required
'o-p32fail4'(Config) -> run_test(Config, "oasis", "p32fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p32fail5
%% Description:
%%   yes or no must be lower case
'o-p32fail5'(Config) -> run_test(Config, "oasis", "p32fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p39fail1
%% Description:
%%   start-tag requires end-tag
'o-p39fail1'(Config) -> run_test(Config, "oasis", "p39fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p39fail2
%% Description:
%%   end-tag requires start-tag
'o-p39fail2'(Config) -> run_test(Config, "oasis", "p39fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p39fail3
%% Description:
%%   XML documents contain one or more elements
'o-p39fail3'(Config) -> run_test(Config, "oasis", "p39fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p39fail4
%% Description:
%%   XML declarations must be correctly terminated
'o-p39fail4'(Config) -> run_test(Config, "oasis", "p39fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p39fail5
%% Description:
%%   XML declarations must be correctly terminated
'o-p39fail5'(Config) -> run_test(Config, "oasis", "p39fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p40fail1
%% Description:
%%   S is required between attributes
'o-p40fail1'(Config) -> run_test(Config, "oasis", "p40fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p40fail2
%% Description:
%%   tags start with names, not nmtokens
'o-p40fail2'(Config) -> run_test(Config, "oasis", "p40fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p40fail3
%% Description:
%%   tags start with names, not nmtokens
'o-p40fail3'(Config) -> run_test(Config, "oasis", "p40fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p40fail4
%% Description:
%%   no space before name
'o-p40fail4'(Config) -> run_test(Config, "oasis", "p40fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p41fail1
%% Description:
%%   quotes are required (contrast with SGML)
'o-p41fail1'(Config) -> run_test(Config, "oasis", "p41fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p41fail2
%% Description:
%%   attribute name is required (contrast with SGML)
'o-p41fail2'(Config) -> run_test(Config, "oasis", "p41fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p41fail3
%% Description:
%%   Eq required
'o-p41fail3'(Config) -> run_test(Config, "oasis", "p41fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p42fail1
%% Description:
%%   no space before name
'o-p42fail1'(Config) -> run_test(Config, "oasis", "p42fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p42fail2
%% Description:
%%   cannot end with "/>"
'o-p42fail2'(Config) -> run_test(Config, "oasis", "p42fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p42fail3
%% Description:
%%   no NET (contrast with SGML)
'o-p42fail3'(Config) -> run_test(Config, "oasis", "p42fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p43fail1
%% Description:
%%   no non-comment declarations
'o-p43fail1'(Config) -> run_test(Config, "oasis", "p43fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p43fail2
%% Description:
%%   no conditional sections
'o-p43fail2'(Config) -> run_test(Config, "oasis", "p43fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p43fail3
%% Description:
%%   no conditional sections
'o-p43fail3'(Config) -> run_test(Config, "oasis", "p43fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p44fail1
%% Description:
%%   Illegal space before Empty element tag.
'o-p44fail1'(Config) -> run_test(Config, "oasis", "p44fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p44fail2
%% Description:
%%   Illegal space after Empty element tag.
'o-p44fail2'(Config) -> run_test(Config, "oasis", "p44fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p44fail3
%% Description:
%%   Illegal comment in Empty element tag.
'o-p44fail3'(Config) -> run_test(Config, "oasis", "p44fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p44fail4
%% Description:
%%   Whitespace required between attributes.
'o-p44fail4'(Config) -> run_test(Config, "oasis", "p44fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p44fail5
%% Description:
%%   Duplicate attribute name is illegal.
'o-p44fail5'(Config) -> run_test(Config, "oasis", "p44fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p45fail1
%% Description:
%%   ELEMENT must be upper case.
'o-p45fail1'(Config) -> run_test(Config, "oasis", "p45fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p45fail2
%% Description:
%%   S before contentspec is required.
'o-p45fail2'(Config) -> run_test(Config, "oasis", "p45fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p45fail3
%% Description:
%%   only one content spec
'o-p45fail3'(Config) -> run_test(Config, "oasis", "p45fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p45fail4
%% Description:
%%   no comments in declarations (contrast with SGML)
'o-p45fail4'(Config) -> run_test(Config, "oasis", "p45fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail1
%% Description:
%%   no parens on declared content
'o-p46fail1'(Config) -> run_test(Config, "oasis", "p46fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail2
%% Description:
%%   no inclusions (contrast with SGML)
'o-p46fail2'(Config) -> run_test(Config, "oasis", "p46fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail3
%% Description:
%%   no exclusions (contrast with SGML)
'o-p46fail3'(Config) -> run_test(Config, "oasis", "p46fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail4
%% Description:
%%   no space before occurrence
'o-p46fail4'(Config) -> run_test(Config, "oasis", "p46fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail5
%% Description:
%%   single group
'o-p46fail5'(Config) -> run_test(Config, "oasis", "p46fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p46fail6
%% Description:
%%   can't be both declared and modeled
'o-p46fail6'(Config) -> run_test(Config, "oasis", "p46fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p47fail1
%% Description:
%%   Invalid operator '|' must match previous operator ','
'o-p47fail1'(Config) -> run_test(Config, "oasis", "p47fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p47fail2
%% Description:
%%   Illegal character '-' in Element-content model
'o-p47fail2'(Config) -> run_test(Config, "oasis", "p47fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p47fail3
%% Description:
%%   Optional character must follow a name or list
'o-p47fail3'(Config) -> run_test(Config, "oasis", "p47fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p47fail4
%% Description:
%%   Illegal space before optional character
'o-p47fail4'(Config) -> run_test(Config, "oasis", "p47fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p48fail1
%% Description:
%%   Illegal space before optional character
'o-p48fail1'(Config) -> run_test(Config, "oasis", "p48fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p48fail2
%% Description:
%%   Illegal space before optional character
'o-p48fail2'(Config) -> run_test(Config, "oasis", "p48fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p49fail1
%% Description:
%%   connectors must match
'o-p49fail1'(Config) -> run_test(Config, "oasis", "p49fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p50fail1
%% Description:
%%   connectors must match
'o-p50fail1'(Config) -> run_test(Config, "oasis", "p50fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail1
%% Description:
%%   occurrence on #PCDATA group must be *
'o-p51fail1'(Config) -> run_test(Config, "oasis", "p51fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail2
%% Description:
%%   occurrence on #PCDATA group must be *
'o-p51fail2'(Config) -> run_test(Config, "oasis", "p51fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail3
%% Description:
%%   #PCDATA must come first
'o-p51fail3'(Config) -> run_test(Config, "oasis", "p51fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail4
%% Description:
%%   occurrence on #PCDATA group must be *
'o-p51fail4'(Config) -> run_test(Config, "oasis", "p51fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail5
%% Description:
%%   only '|' connectors
'o-p51fail5'(Config) -> run_test(Config, "oasis", "p51fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail6
%% Description:
%%   Only '|' connectors and occurrence on #PCDATA group must be *
'o-p51fail6'(Config) -> run_test(Config, "oasis", "p51fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p51fail7
%% Description:
%%   no nested groups
'o-p51fail7'(Config) -> run_test(Config, "oasis", "p51fail7.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p52fail1
%% Description:
%%   A name is required
'o-p52fail1'(Config) -> run_test(Config, "oasis", "p52fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p52fail2
%% Description:
%%   A name is required
'o-p52fail2'(Config) -> run_test(Config, "oasis", "p52fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p53fail1
%% Description:
%%   S is required before default
'o-p53fail1'(Config) -> run_test(Config, "oasis", "p53fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p53fail2
%% Description:
%%   S is required before type
'o-p53fail2'(Config) -> run_test(Config, "oasis", "p53fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p53fail3
%% Description:
%%   type is required
'o-p53fail3'(Config) -> run_test(Config, "oasis", "p53fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p53fail4
%% Description:
%%   default is required
'o-p53fail4'(Config) -> run_test(Config, "oasis", "p53fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p53fail5
%% Description:
%%   name is requried
'o-p53fail5'(Config) -> run_test(Config, "oasis", "p53fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p54fail1
%% Description:
%%   don't pass unknown attribute types
'o-p54fail1'(Config) -> run_test(Config, "oasis", "p54fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p55fail1
%% Description:
%%   must be upper case
'o-p55fail1'(Config) -> run_test(Config, "oasis", "p55fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p56fail1
%% Description:
%%   no IDS type
'o-p56fail1'(Config) -> run_test(Config, "oasis", "p56fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p56fail2
%% Description:
%%   no NUMBER type
'o-p56fail2'(Config) -> run_test(Config, "oasis", "p56fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p56fail3
%% Description:
%%   no NAME type
'o-p56fail3'(Config) -> run_test(Config, "oasis", "p56fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p56fail4
%% Description:
%%   no ENTITYS type - types must be upper case
'o-p56fail4'(Config) -> run_test(Config, "oasis", "p56fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p56fail5
%% Description:
%%   types must be upper case
'o-p56fail5'(Config) -> run_test(Config, "oasis", "p56fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p57fail1
%% Description:
%%   no keyword for NMTOKEN enumeration
'o-p57fail1'(Config) -> run_test(Config, "oasis", "p57fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail1
%% Description:
%%   at least one value required
'o-p58fail1'(Config) -> run_test(Config, "oasis", "p58fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail2
%% Description:
%%   separator must be '|'
'o-p58fail2'(Config) -> run_test(Config, "oasis", "p58fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail3
%% Description:
%%   notations are NAMEs, not NMTOKENs -- note: Leaving the invalid
%%   notation undeclared would cause a validating parser to fail without
%%   checking the name syntax, so the notation is declared with an
%%   invalid name. A parser that reports error positions should report an
%%   error at the AttlistDecl on line 6, before reaching the notation
%%   declaration.
'o-p58fail3'(Config) -> run_test(Config, "oasis", "p58fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail4
%% Description:
%%   NOTATION must be upper case
'o-p58fail4'(Config) -> run_test(Config, "oasis", "p58fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail5
%% Description:
%%   S after keyword is required
'o-p58fail5'(Config) -> run_test(Config, "oasis", "p58fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail6
%% Description:
%%   parentheses are require
'o-p58fail6'(Config) -> run_test(Config, "oasis", "p58fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail7
%% Description:
%%   values are unquoted
'o-p58fail7'(Config) -> run_test(Config, "oasis", "p58fail7.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p58fail8
%% Description:
%%   values are unquoted
'o-p58fail8'(Config) -> run_test(Config, "oasis", "p58fail8.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p59fail1
%% Description:
%%   at least one required
'o-p59fail1'(Config) -> run_test(Config, "oasis", "p59fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p59fail2
%% Description:
%%   separator must be ","
'o-p59fail2'(Config) -> run_test(Config, "oasis", "p59fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p59fail3
%% Description:
%%   values are unquoted
'o-p59fail3'(Config) -> run_test(Config, "oasis", "p59fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p60fail1
%% Description:
%%   keywords must be upper case
'o-p60fail1'(Config) -> run_test(Config, "oasis", "p60fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p60fail2
%% Description:
%%   S is required after #FIXED
'o-p60fail2'(Config) -> run_test(Config, "oasis", "p60fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p60fail3
%% Description:
%%   only #FIXED has both keyword and value
'o-p60fail3'(Config) -> run_test(Config, "oasis", "p60fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p60fail4
%% Description:
%%   #FIXED required value
'o-p60fail4'(Config) -> run_test(Config, "oasis", "p60fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p60fail5
%% Description:
%%   only one default type
'o-p60fail5'(Config) -> run_test(Config, "oasis", "p60fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p61fail1
%% Entities: parameter
%% Description:
%%   no other types, including TEMP, which is valid in SGML
'o-p61fail1'(Config) -> run_test(Config, "oasis", "p61fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p62fail1
%% Entities: parameter
%% Description:
%%   INCLUDE must be upper case
'o-p62fail1'(Config) -> run_test(Config, "oasis", "p62fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p62fail2
%% Entities: parameter
%% Description:
%%   no spaces in terminating delimiter
'o-p62fail2'(Config) -> run_test(Config, "oasis", "p62fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p63fail1
%% Entities: parameter
%% Description:
%%   IGNORE must be upper case
'o-p63fail1'(Config) -> run_test(Config, "oasis", "p63fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p63fail2
%% Entities: parameter
%% Description:
%%   delimiters must be balanced
'o-p63fail2'(Config) -> run_test(Config, "oasis", "p63fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p64fail1
%% Entities: parameter
%% Description:
%%   section delimiters must balance
'o-p64fail1'(Config) -> run_test(Config, "oasis", "p64fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p64fail2
%% Entities: parameter
%% Description:
%%   section delimiters must balance
'o-p64fail2'(Config) -> run_test(Config, "oasis", "p64fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail1
%% Description:
%%   terminating ';' is required
'o-p66fail1'(Config) -> run_test(Config, "oasis", "p66fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail2
%% Description:
%%   no S after '&#'
'o-p66fail2'(Config) -> run_test(Config, "oasis", "p66fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail3
%% Description:
%%   no hex digits in numeric reference
'o-p66fail3'(Config) -> run_test(Config, "oasis", "p66fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail4
%% Description:
%%   only hex digits in hex references
'o-p66fail4'(Config) -> run_test(Config, "oasis", "p66fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail5
%% Description:
%%   no references to non-characters
'o-p66fail5'(Config) -> run_test(Config, "oasis", "p66fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p66fail6
%% Description:
%%   no references to non-characters
'o-p66fail6'(Config) -> run_test(Config, "oasis", "p66fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p68fail1
%% Description:
%%   terminating ';' is required
'o-p68fail1'(Config) -> run_test(Config, "oasis", "p68fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p68fail2
%% Description:
%%   no S after '&'
'o-p68fail2'(Config) -> run_test(Config, "oasis", "p68fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p68fail3
%% Description:
%%   no S before ';'
'o-p68fail3'(Config) -> run_test(Config, "oasis", "p68fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p69fail1
%% Description:
%%   terminating ';' is required
'o-p69fail1'(Config) -> run_test(Config, "oasis", "p69fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p69fail2
%% Description:
%%   no S after '%'
'o-p69fail2'(Config) -> run_test(Config, "oasis", "p69fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p69fail3
%% Description:
%%   no S before ';'
'o-p69fail3'(Config) -> run_test(Config, "oasis", "p69fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p70fail1
%% Description:
%%   This is neither
'o-p70fail1'(Config) -> run_test(Config, "oasis", "p70fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p71fail1
%% Description:
%%   S is required before EntityDef
'o-p71fail1'(Config) -> run_test(Config, "oasis", "p71fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p71fail2
%% Description:
%%   Entity name is a Name, not an NMToken
'o-p71fail2'(Config) -> run_test(Config, "oasis", "p71fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p71fail3
%% Description:
%%   no S after "<!"
'o-p71fail3'(Config) -> run_test(Config, "oasis", "p71fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p71fail4
%% Description:
%%   S is required after "<!ENTITY"
'o-p71fail4'(Config) -> run_test(Config, "oasis", "p71fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p72fail1
%% Description:
%%   S is required after "<!ENTITY"
'o-p72fail1'(Config) -> run_test(Config, "oasis", "p72fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p72fail2
%% Description:
%%   S is required after '%'
'o-p72fail2'(Config) -> run_test(Config, "oasis", "p72fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p72fail3
%% Description:
%%   S is required after name
'o-p72fail3'(Config) -> run_test(Config, "oasis", "p72fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p72fail4
%% Description:
%%   Entity name is a name, not an NMToken
'o-p72fail4'(Config) -> run_test(Config, "oasis", "p72fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p73fail1
%% Description:
%%   No typed replacement text
'o-p73fail1'(Config) -> run_test(Config, "oasis", "p73fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p73fail2
%% Description:
%%   Only one replacement value
'o-p73fail2'(Config) -> run_test(Config, "oasis", "p73fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p73fail3
%% Description:
%%   No NDataDecl on replacement text
'o-p73fail3'(Config) -> run_test(Config, "oasis", "p73fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p73fail4
%% Description:
%%   Value is required
'o-p73fail4'(Config) -> run_test(Config, "oasis", "p73fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p73fail5
%% Description:
%%   No NDataDecl without value
'o-p73fail5'(Config) -> run_test(Config, "oasis", "p73fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p74fail1
%% Description:
%%   no NDataDecls on parameter entities
'o-p74fail1'(Config) -> run_test(Config, "oasis", "p74fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p74fail2
%% Description:
%%   value is required
'o-p74fail2'(Config) -> run_test(Config, "oasis", "p74fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p74fail3
%% Description:
%%   only one value
'o-p74fail3'(Config) -> run_test(Config, "oasis", "p74fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail1
%% Description:
%%   S required after "PUBLIC"
'o-p75fail1'(Config) -> run_test(Config, "oasis", "p75fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail2
%% Description:
%%   S required after "SYSTEM"
'o-p75fail2'(Config) -> run_test(Config, "oasis", "p75fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail3
%% Description:
%%   S required between literals
'o-p75fail3'(Config) -> run_test(Config, "oasis", "p75fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail4
%% Description:
%%   "SYSTEM" implies only one literal
'o-p75fail4'(Config) -> run_test(Config, "oasis", "p75fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail5
%% Description:
%%   only one keyword
'o-p75fail5'(Config) -> run_test(Config, "oasis", "p75fail5.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p75fail6
%% Description:
%%   "PUBLIC" requires two literals (contrast with SGML)
'o-p75fail6'(Config) -> run_test(Config, "oasis", "p75fail6.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p76fail1
%% Description:
%%   S is required before "NDATA"
'o-p76fail1'(Config) -> run_test(Config, "oasis", "p76fail1.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p76fail2
%% Description:
%%   "NDATA" is upper-case
'o-p76fail2'(Config) -> run_test(Config, "oasis", "p76fail2.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p76fail3
%% Description:
%%   notation name is required
'o-p76fail3'(Config) -> run_test(Config, "oasis", "p76fail3.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p76fail4
%% Description:
%%   notation names are Names
'o-p76fail4'(Config) -> run_test(Config, "oasis", "p76fail4.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: o-p11pass1
%% Description:
%%   system literals may not contain URI fragments
'o-p11pass1'(Config) -> run_test(Config, "oasis", "p11pass1.xml", "error").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: IBM XML 1.0 Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P28-ibm28i01.xml
%% Output: invalid/P28/out/ibm28i01.xml
%% Description:
%%   The test violates VC:Root Element Type in P28. The Name in the
%%   document type declaration does not match the element type of the
%%   root element.
'ibm-invalid-P28-ibm28i01'(Config) ->
    run_test(Config, "ibm", "invalid/P28/ibm28i01.xml", "invalid", "invalid/P28/out/ibm28i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P32-ibm32i01.xml
%% Entities: parameter
%% Output: invalid/P32/out/ibm32i01.xml
%% Description:
%%   This test violates VC: Standalone Document Declaration in P32. The
%%   standalone document declaration has the value yes, BUT there is an
%%   external markup declaration of attributes with default values, and
%%   the associated element appears in the document with specified values
%%   for those attributes.
'ibm-invalid-P32-ibm32i01'(Config) ->
    run_test(Config, "ibm", "invalid/P32/ibm32i01.xml", "invalid", "invalid/P32/out/ibm32i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P32-ibm32i03.xml
%% Entities: parameter
%% Output: invalid/P32/out/ibm32i03.xml
%% Description:
%%   This test violates VC: Standalone Document Declaration in P32. The
%%   standalone document declaration has the value yes, BUT there is an
%%   external markup declaration of attributes with values that will
%%   change if normalized.
'ibm-invalid-P32-ibm32i03'(Config) ->
    run_test(Config, "ibm", "invalid/P32/ibm32i03.xml", "invalid", "invalid/P32/out/ibm32i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P32-ibm32i04.xml
%% Entities: parameter
%% Output: invalid/P32/out/ibm32i04.xml
%% Description:
%%   This test violates VC: Standalone Document Declaration in P32. The
%%   standalone document declaration has the value yes, BUT there is an
%%   external markup declaration of element with element content, and
%%   white space occurs directly within the mixed content.
'ibm-invalid-P32-ibm32i04'(Config) ->
    run_test(Config, "ibm", "invalid/P32/ibm32i04.xml", "invalid", "invalid/P32/out/ibm32i04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P39-ibm39i01.xml
%% Output: invalid/P39/out/ibm39i01.xml
%% Description:
%%   This test violates VC: Element Valid in P39. Element a is declared
%%   empty in DTD, but has content in the document.
'ibm-invalid-P39-ibm39i01'(Config) ->
    run_test(Config, "ibm", "invalid/P39/ibm39i01.xml", "invalid", "invalid/P39/out/ibm39i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P39-ibm39i02.xml
%% Output: invalid/P39/out/ibm39i02.xml
%% Description:
%%   This test violates VC: Element Valid in P39. root is declared only
%%   having element children in DTD, but have text content in the
%%   document.
'ibm-invalid-P39-ibm39i02'(Config) ->
    run_test(Config, "ibm", "invalid/P39/ibm39i02.xml", "invalid", "invalid/P39/out/ibm39i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P39-ibm39i03.xml
%% Output: invalid/P39/out/ibm39i03.xml
%% Description:
%%   This test violates VC: Element Valid in P39. Illegal elements are
%%   inserted in b's content of Mixed type.
'ibm-invalid-P39-ibm39i03'(Config) ->
    run_test(Config, "ibm", "invalid/P39/ibm39i03.xml", "invalid", "invalid/P39/out/ibm39i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P39-ibm39i04.xml
%% Output: invalid/P39/out/ibm39i04.xml
%% Description:
%%   This test violates VC: Element Valid in P39. Element c has
%%   undeclared element as its content of ANY type
'ibm-invalid-P39-ibm39i04'(Config) ->
    run_test(Config, "ibm", "invalid/P39/ibm39i04.xml", "invalid", "invalid/P39/out/ibm39i04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P41-ibm41i01.xml
%% Output: invalid/P41/out/ibm41i01.xml
%% Description:
%%   This test violates VC: Attribute Value Type in P41. attr1 for
%%   Element b is not declared.
'ibm-invalid-P41-ibm41i01'(Config) ->
    run_test(Config, "ibm", "invalid/P41/ibm41i01.xml", "invalid", "invalid/P41/out/ibm41i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P41-ibm41i02.xml
%% Output: invalid/P41/out/ibm41i02.xml
%% Description:
%%   This test violates VC: Attribute Value Type in P41. attr3 for
%%   Element b is given a value that does not match the declaration in
%%   the DTD.
'ibm-invalid-P41-ibm41i02'(Config) ->
    run_test(Config, "ibm", "invalid/P41/ibm41i02.xml", "invalid", "invalid/P41/out/ibm41i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P45-ibm45i01.xml
%% Output: invalid/P45/out/ibm45i01.xml
%% Description:
%%   This test violates VC: Unique Element Type Declaration. Element
%%   not_unique has been declared 3 time in the DTD.
'ibm-invalid-P45-ibm45i01'(Config) ->
    run_test(Config, "ibm", "invalid/P45/ibm45i01.xml", "invalid", "invalid/P45/out/ibm45i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P49-ibm49i01.xml
%% Entities: parameter
%% Output: invalid/P49/out/ibm49i01.xml
%% Description:
%%   Violates VC:Proper Group/PE Nesting in P49. Open and close
%%   parenthesis for a choice content model are in different PE replace
%%   Texts.
'ibm-invalid-P49-ibm49i01'(Config) ->
    run_test(Config, "ibm", "invalid/P49/ibm49i01.xml", "invalid", "invalid/P49/out/ibm49i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P50-ibm50i01.xml
%% Entities: parameter
%% Output: invalid/P50/out/ibm50i01.xml
%% Description:
%%   Violates VC:Proper Group/PE Nesting in P50. Open and close
%%   parenthesis for a seq content model are in different PE replace
%%   Texts.
'ibm-invalid-P50-ibm50i01'(Config) ->
    run_test(Config, "ibm", "invalid/P50/ibm50i01.xml", "invalid", "invalid/P50/out/ibm50i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P51-ibm51i01.xml
%% Entities: parameter
%% Output: invalid/P51/out/ibm51i01.xml
%% Description:
%%   Violates VC:Proper Group/PE Nesting in P51. Open and close
%%   parenthesis for a Mixed content model are in different PE replace
%%   Texts.
'ibm-invalid-P51-ibm51i01'(Config) ->
    run_test(Config, "ibm", "invalid/P51/ibm51i01.xml", "invalid", "invalid/P51/out/ibm51i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P51-ibm51i03.xml
%% Output: invalid/P51/out/ibm51i03.xml
%% Description:
%%   Violates VC:No Duplicate Types in P51. Element a appears twice in
%%   the Mixed content model of Element e.
'ibm-invalid-P51-ibm51i03'(Config) ->
    run_test(Config, "ibm", "invalid/P51/ibm51i03.xml", "invalid", "invalid/P51/out/ibm51i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i01.xml
%% Output: invalid/P56/out/ibm56i01.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ID. The value
%%   of the ID attribute "UniqueName" is "@999" which does not meet the
%%   Name production.
'ibm-invalid-P56-ibm56i01'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i01.xml", "invalid", "invalid/P56/out/ibm56i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i02.xml
%% Output: invalid/P56/out/ibm56i02.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ID. The two ID
%%   attributes "attr" and "UniqueName" have the same value "Ac999" for
%%   the element "b" and the element "tokenizer".
'ibm-invalid-P56-ibm56i02'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i02.xml", "invalid", "invalid/P56/out/ibm56i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i03.xml
%% Output: invalid/P56/out/ibm56i03.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ID Attribute
%%   Default. The "#FIXED" occurs in the DefaultDecl for the ID attribute
%%   "UniqueName".
'ibm-invalid-P56-ibm56i03'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i03.xml", "invalid", "invalid/P56/out/ibm56i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i05.xml
%% Output: invalid/P56/out/ibm56i05.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ID Attribute
%%   Default. The constant string "BOGUS" occurs in the DefaultDecl for
%%   the ID attribute "UniqueName".
'ibm-invalid-P56-ibm56i05'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i05.xml", "invalid", "invalid/P56/out/ibm56i05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i06.xml
%% Output: invalid/P56/out/ibm56i06.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: One ID per
%%   Element Type. The element "a" has two ID attributes "first" and
%%   "second".
'ibm-invalid-P56-ibm56i06'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i06.xml", "invalid", "invalid/P56/out/ibm56i06.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i07.xml
%% Output: invalid/P56/out/ibm56i07.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: IDREF. The
%%   value of the IDREF attribute "reference" is "@456" which does not
%%   meet the Name production.
'ibm-invalid-P56-ibm56i07'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i07.xml", "invalid", "invalid/P56/out/ibm56i07.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i08.xml
%% Output: invalid/P56/out/ibm56i08.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: IDREF. The
%%   value of the IDREF attribute "reference" is "BC456" which does not
%%   match the value assigned to any ID attributes.
'ibm-invalid-P56-ibm56i08'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i08.xml", "invalid", "invalid/P56/out/ibm56i08.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i09.xml
%% Output: invalid/P56/out/ibm56i09.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: IDREFS. The
%%   value of the IDREFS attribute "reference" is "AC456 #567" which does
%%   not meet the Names production.
'ibm-invalid-P56-ibm56i09'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i09.xml", "invalid", "invalid/P56/out/ibm56i09.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i10.xml
%% Output: invalid/P56/out/ibm56i10.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: IDREFS. The
%%   value of the IDREFS attribute "reference" is "EF456 DE355" which
%%   does not match the values assigned to two ID attributes.
'ibm-invalid-P56-ibm56i10'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i10.xml", "invalid", "invalid/P56/out/ibm56i10.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i11.xml
%% Output: invalid/P56/out/ibm56i11.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Entity Name.
%%   The value of the ENTITY attribute "sun" is "ima ge" which does not
%%   meet the Name production.
'ibm-invalid-P56-ibm56i11'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i11.xml", "invalid", "invalid/P56/out/ibm56i11.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i12.xml
%% Output: invalid/P56/out/ibm56i12.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Entity Name.
%%   The value of the ENTITY attribute "sun" is "notimage" which does not
%%   match the name of any unparsed entity declared.
'ibm-invalid-P56-ibm56i12'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i12.xml", "invalid", "invalid/P56/out/ibm56i12.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i13.xml
%% Output: invalid/P56/out/ibm56i13.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Entity Name.
%%   The value of the ENTITY attribute "sun" is "parsedentity" which
%%   matches the name of a parsed entity instead of an unparsed entity
%%   declared.
'ibm-invalid-P56-ibm56i13'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i13.xml", "invalid", "invalid/P56/out/ibm56i13.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i14.xml
%% Output: invalid/P56/out/ibm56i14.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Entity Name.
%%   The value of the ENTITIES attribute "sun" is "#image1 @image" which
%%   does not meet the Names production.
'ibm-invalid-P56-ibm56i14'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i14.xml", "invalid", "invalid/P56/out/ibm56i14.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i15.xml
%% Output: invalid/P56/out/ibm56i15.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ENTITIES. The
%%   value of the ENTITIES attribute "sun" is "image3 image4" which does
%%   not match the names of two unparsed entities declared.
'ibm-invalid-P56-ibm56i15'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i15.xml", "invalid", "invalid/P56/out/ibm56i15.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i16.xml
%% Output: invalid/P56/out/ibm56i16.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: ENTITIES. The
%%   value of the ENTITIES attribute "sun" is "parsedentity1
%%   parsedentity2" which matches the names of two parsed entities
%%   instead of two unparsed entities declared.
'ibm-invalid-P56-ibm56i16'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i16.xml", "invalid", "invalid/P56/out/ibm56i16.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i17.xml
%% Output: invalid/P56/out/ibm56i17.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Name Token. The
%%   value of the NMTOKEN attribute "thistoken" is "x : image" which does
%%   not meet the Nmtoken production.
'ibm-invalid-P56-ibm56i17'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i17.xml", "invalid", "invalid/P56/out/ibm56i17.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P56-ibm56i18.xml
%% Output: invalid/P56/out/ibm56i18.xml
%% Description:
%%   Tests invalid TokenizedType which is against P56 VC: Name Token. The
%%   value of the NMTOKENS attribute "thistoken" is "@lang y: #country"
%%   which does not meet the Nmtokens production.
'ibm-invalid-P56-ibm56i18'(Config) ->
    run_test(Config, "ibm", "invalid/P56/ibm56i18.xml", "invalid", "invalid/P56/out/ibm56i18.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P58-ibm58i01.xml
%% Output: invalid/P58/out/ibm58i01.xml
%% Description:
%%   Tests invalid NotationType which is against P58 VC: Notation
%%   Attributes. The attribute "content-encoding" with value "raw" is not
%%   a value from the list "(base64|uuencode)".
'ibm-invalid-P58-ibm58i01'(Config) ->
    run_test(Config, "ibm", "invalid/P58/ibm58i01.xml", "invalid", "invalid/P58/out/ibm58i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P58-ibm58i02.xml
%% Output: invalid/P58/out/ibm58i02.xml
%% Description:
%%   Tests invalid NotationType which is against P58 VC: Notation
%%   Attributes. The attribute "content-encoding" with value "raw" is a
%%   value from the list "(base64|uuencode|raw|ascii)", but "raw" is not
%%   a declared notation.
'ibm-invalid-P58-ibm58i02'(Config) ->
    run_test(Config, "ibm", "invalid/P58/ibm58i02.xml", "invalid", "invalid/P58/out/ibm58i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P59-ibm59i01.xml
%% Output: invalid/P59/out/ibm59i01.xml
%% Description:
%%   Tests invalid Enumeration which is against P59 VC: Enumeration. The
%%   value of the attribute is "ONE" which matches neither "one" nor
%%   "two" as declared in the Enumeration in the AttDef in the
%%   AttlistDecl.
'ibm-invalid-P59-ibm59i01'(Config) ->
    run_test(Config, "ibm", "invalid/P59/ibm59i01.xml", "invalid", "invalid/P59/out/ibm59i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P60-ibm60i01.xml
%% Output: invalid/P60/out/ibm60i01.xml
%% Description:
%%   Tests invalid DefaultDecl which is against P60 VC: Required
%%   Attribute. The attribute "chapter" for the element "two" is declared
%%   as #REQUIRED in the DefaultDecl in the AttlistDecl, but the value of
%%   this attribute is not given.
'ibm-invalid-P60-ibm60i01'(Config) ->
    run_test(Config, "ibm", "invalid/P60/ibm60i01.xml", "invalid", "invalid/P60/out/ibm60i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P60-ibm60i02.xml
%% Output: invalid/P60/out/ibm60i02.xml
%% Description:
%%   Tests invalid DefaultDecl which is against P60 VC: Fixed Attribute
%%   Default.. The attribute "chapter" for the element "one" is declared
%%   as #FIXED with the given value "Introduction" in the DefaultDecl in
%%   the AttlistDecl, but the value of a instance of this attribute is
%%   assigned to "JavaBeans".
'ibm-invalid-P60-ibm60i02'(Config) ->
    run_test(Config, "ibm", "invalid/P60/ibm60i02.xml", "invalid", "invalid/P60/out/ibm60i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P60-ibm60i03.xml
%% Output: invalid/P60/out/ibm60i03.xml
%% Description:
%%   Tests invalid DefaultDecl which is against P60 VC: Attribute Default
%%   Legal. The declared default value "c" is not legal for the type
%%   (a|b) in the AttDef in the AttlistDecl.
'ibm-invalid-P60-ibm60i03'(Config) ->
    run_test(Config, "ibm", "invalid/P60/ibm60i03.xml", "invalid", "invalid/P60/out/ibm60i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P60-ibm60i04.xml
%% Output: invalid/P60/out/ibm60i04.xml
%% Description:
%%   Tests invalid DefaultDecl which is against P60 VC: Attribute Default
%%   Legal. The declared default value "@#$" is not legal for the type
%%   NMTOKEN the AttDef in the AttlistDecl.
'ibm-invalid-P60-ibm60i04'(Config) ->
    run_test(Config, "ibm", "invalid/P60/ibm60i04.xml", "invalid", "invalid/P60/out/ibm60i04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P68-ibm68i01.xml
%% Entities: parameter
%% Output: invalid/P68/out/ibm68i01.xml
%% Description:
%%   Tests invalid EntityRef which is against P68 VC: Entity Declared.
%%   The GE with the name "ge2" is referred in the file ibm68i01.dtd",
%%   but not declared.
'ibm-invalid-P68-ibm68i01'(Config) ->
    run_test(Config, "ibm", "invalid/P68/ibm68i01.xml", "error", "invalid/P68/out/ibm68i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P68-ibm68i02.xml
%% Entities: parameter
%% Output: invalid/P68/out/ibm68i02.xml
%% Description:
%%   Tests invalid EntityRef which is against P68 VC: Entity Declared.
%%   The GE with the name "ge1" is referred before declared in the file
%%   ibm68i01.dtd".
'ibm-invalid-P68-ibm68i02'(Config) ->
    run_test(Config, "ibm", "invalid/P68/ibm68i02.xml", "error", "invalid/P68/out/ibm68i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P68-ibm68i03.xml
%% Entities: both
%% Output: invalid/P68/out/ibm68i03.xml
%% Description:
%%   Tests invalid EntityRef which is against P68 VC: Entity Declared.
%%   The GE with the name "ge2" is referred in the file ibm68i03.ent",
%%   but not declared.
'ibm-invalid-P68-ibm68i03'(Config) ->
    run_test(Config, "ibm", "invalid/P68/ibm68i03.xml", "error", "invalid/P68/out/ibm68i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P68-ibm68i04.xml
%% Entities: both
%% Output: invalid/P68/out/ibm68i04.xml
%% Description:
%%   Tests invalid EntityRef which is against P68 VC: Entity Declared.
%%   The GE with the name "ge1" is referred before declared in the file
%%   ibm68i04.ent".
'ibm-invalid-P68-ibm68i04'(Config) ->
    run_test(Config, "ibm", "invalid/P68/ibm68i04.xml", "error", "invalid/P68/out/ibm68i04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P69-ibm69i01.xml
%% Entities: parameter
%% Output: invalid/P69/out/ibm69i01.xml
%% Description:
%%   Tests invalid PEReference which is against P69 VC: Entity Declared.
%%   The Name "pe2" in the PEReference in the file ibm69i01.dtd does not
%%   match the Name of any declared PE.
'ibm-invalid-P69-ibm69i01'(Config) ->
    run_test(Config, "ibm", "invalid/P69/ibm69i01.xml", "error", "invalid/P69/out/ibm69i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P69-ibm69i02.xml
%% Entities: parameter
%% Output: invalid/P69/out/ibm69i02.xml
%% Description:
%%   Tests invalid PEReference which is against P69 VC: Entity Declared.
%%   The PE with the name "pe1" is referred before declared in the file
%%   ibm69i02.dtd
'ibm-invalid-P69-ibm69i02'(Config) ->
    run_test(Config, "ibm", "invalid/P69/ibm69i02.xml", "error", "invalid/P69/out/ibm69i02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P69-ibm69i03.xml
%% Entities: both
%% Output: invalid/P69/out/ibm69i03.xml
%% Description:
%%   Tests invalid PEReference which is against P69 VC: Entity Declared.
%%   The Name "pe3" in the PEReference in the file ibm69i03.ent does not
%%   match the Name of any declared PE.
'ibm-invalid-P69-ibm69i03'(Config) ->
    run_test(Config, "ibm", "invalid/P69/ibm69i03.xml", "error", "invalid/P69/out/ibm69i03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P69-ibm69i04.xml
%% Entities: both
%% Output: invalid/P69/out/ibm69i04.xml
%% Description:
%%   Tests invalid PEReference which is against P69 VC: Entity Declared.
%%   The PE with the name "pe2" is referred before declared in the file
%%   ibm69i04.ent.
'ibm-invalid-P69-ibm69i04'(Config) ->
    run_test(Config, "ibm", "invalid/P69/ibm69i04.xml", "error", "invalid/P69/out/ibm69i04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P76-ibm76i01.xml
%% Output: invalid/P76/out/ibm76i01.xml
%% Description:
%%   Tests invalid NDataDecl which is against P76 VC: Notation declared.
%%   The Name "JPGformat" in the NDataDecl in the EntityDecl for "ge2"
%%   does not match the Name of any declared notation.
'ibm-invalid-P76-ibm76i01'(Config) ->
    run_test(Config, "ibm", "invalid/P76/ibm76i01.xml", "invalid", "invalid/P76/out/ibm76i01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P01-ibm01n01.xml
%% Description:
%%   Tests a document with no element. A well-formed document should have
%%   at lease one elements.
'ibm-not-wf-P01-ibm01n01'(Config) -> run_test(Config, "ibm", "not-wf/P01/ibm01n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P01-ibm01n02.xml
%% Description:
%%   Tests a document with wrong ordering of its prolog and element. The
%%   element occurs before the xml declaration and the DTD.
'ibm-not-wf-P01-ibm01n02'(Config) -> run_test(Config, "ibm", "not-wf/P01/ibm01n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P01-ibm01n03.xml
%% Description:
%%   Tests a document with wrong combination of misc and element. One PI
%%   occurs between two elements.
'ibm-not-wf-P01-ibm01n03'(Config) -> run_test(Config, "ibm", "not-wf/P01/ibm01n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n01.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x00
'ibm-not-wf-P02-ibm02n01'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n02.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x01
'ibm-not-wf-P02-ibm02n02'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n03.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x02
'ibm-not-wf-P02-ibm02n03'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n04.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x03
'ibm-not-wf-P02-ibm02n04'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n05.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x04
'ibm-not-wf-P02-ibm02n05'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n06.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x05
'ibm-not-wf-P02-ibm02n06'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n07.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x06
'ibm-not-wf-P02-ibm02n07'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n08.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x07
'ibm-not-wf-P02-ibm02n08'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n09.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x08
'ibm-not-wf-P02-ibm02n09'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n10.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x0B
'ibm-not-wf-P02-ibm02n10'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n11.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x0C
'ibm-not-wf-P02-ibm02n11'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n12.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x0E
'ibm-not-wf-P02-ibm02n12'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n13.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x0F
'ibm-not-wf-P02-ibm02n13'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n14.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x10
'ibm-not-wf-P02-ibm02n14'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n15.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x11
'ibm-not-wf-P02-ibm02n15'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n16.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x12
'ibm-not-wf-P02-ibm02n16'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n17.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x13
'ibm-not-wf-P02-ibm02n17'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n18.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x14
'ibm-not-wf-P02-ibm02n18'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n19.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x15
'ibm-not-wf-P02-ibm02n19'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n20.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x16
'ibm-not-wf-P02-ibm02n20'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n21.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x17
'ibm-not-wf-P02-ibm02n21'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n22.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x18
'ibm-not-wf-P02-ibm02n22'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n23.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x19
'ibm-not-wf-P02-ibm02n23'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n24.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1A
'ibm-not-wf-P02-ibm02n24'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n25.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1B
'ibm-not-wf-P02-ibm02n25'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n26.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1C
'ibm-not-wf-P02-ibm02n26'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n27.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1D
'ibm-not-wf-P02-ibm02n27'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n28.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1E
'ibm-not-wf-P02-ibm02n28'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n29.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #x1F
'ibm-not-wf-P02-ibm02n29'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n29.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n30.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #xD800
'ibm-not-wf-P02-ibm02n30'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n30.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n31.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #xDFFF
'ibm-not-wf-P02-ibm02n31'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n31.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n32.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #xFFFE
'ibm-not-wf-P02-ibm02n32'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n32.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P02-ibm02n33.xml
%% Description:
%%   Tests a comment which contains an illegal Char: #xFFFF
'ibm-not-wf-P02-ibm02n33'(Config) -> run_test(Config, "ibm", "not-wf/P02/ibm02n33.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P03-ibm03n01.xml
%% Description:
%%   Tests an end tag which contains an illegal space character #x3000
%%   which follows the element name "book".
'ibm-not-wf-P03-ibm03n01'(Config) -> run_test(Config, "ibm", "not-wf/P03/ibm03n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n01.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x21
'ibm-not-wf-P04-ibm04n01'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n02.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x28
'ibm-not-wf-P04-ibm04n02'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n03.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x29
'ibm-not-wf-P04-ibm04n03'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n04.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x2B
'ibm-not-wf-P04-ibm04n04'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n05.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x2C
'ibm-not-wf-P04-ibm04n05'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n06.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x2F
'ibm-not-wf-P04-ibm04n06'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n07.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x3B
'ibm-not-wf-P04-ibm04n07'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n08.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x3C
'ibm-not-wf-P04-ibm04n08'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n09.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x3D
'ibm-not-wf-P04-ibm04n09'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n10.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x3F
'ibm-not-wf-P04-ibm04n10'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n11.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x5B
'ibm-not-wf-P04-ibm04n11'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n12.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x5C
'ibm-not-wf-P04-ibm04n12'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n13.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x5D
'ibm-not-wf-P04-ibm04n13'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n14.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x5E
'ibm-not-wf-P04-ibm04n14'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n15.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x60
'ibm-not-wf-P04-ibm04n15'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n16.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x7B
'ibm-not-wf-P04-ibm04n16'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n17.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x7C
'ibm-not-wf-P04-ibm04n17'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P04-ibm04n18.xml
%% Description:
%%   Tests an element name which contains an illegal ASCII NameChar.
%%   "IllegalNameChar" is followed by #x7D
'ibm-not-wf-P04-ibm04n18'(Config) -> run_test(Config, "ibm", "not-wf/P04/ibm04n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P05-ibm05n01.xml
%% Description:
%%   Tests an element name which has an illegal first character. An
%%   illegal first character "." is followed by "A_name-starts_with.".
'ibm-not-wf-P05-ibm05n01'(Config) -> run_test(Config, "ibm", "not-wf/P05/ibm05n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P05-ibm05n02.xml
%% Description:
%%   Tests an element name which has an illegal first character. An
%%   illegal first character "-" is followed by "A_name-starts_with-".
'ibm-not-wf-P05-ibm05n02'(Config) -> run_test(Config, "ibm", "not-wf/P05/ibm05n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P05-ibm05n03.xml
%% Description:
%%   Tests an element name which has an illegal first character. An
%%   illegal first character "5" is followed by
%%   "A_name-starts_with_digit".
'ibm-not-wf-P05-ibm05n03'(Config) -> run_test(Config, "ibm", "not-wf/P05/ibm05n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P09-ibm09n01.xml
%% Description:
%%   Tests an internal general entity with an invalid value. The entity
%%   "Fullname" contains "%".
'ibm-not-wf-P09-ibm09n01'(Config) -> run_test(Config, "ibm", "not-wf/P09/ibm09n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P09-ibm09n02.xml
%% Description:
%%   Tests an internal general entity with an invalid value. The entity
%%   "Fullname" contains the ampersand character.
'ibm-not-wf-P09-ibm09n02'(Config) -> run_test(Config, "ibm", "not-wf/P09/ibm09n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P09-ibm09n03.xml
%% Description:
%%   Tests an internal general entity with an invalid value. The entity
%%   "Fullname" contains the double quote character in the middle.
'ibm-not-wf-P09-ibm09n03'(Config) -> run_test(Config, "ibm", "not-wf/P09/ibm09n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P09-ibm09n04.xml
%% Description:
%%   Tests an internal general entity with an invalid value. The closing
%%   bracket (double quote) is missing with the value of the entity
%%   "FullName".
'ibm-not-wf-P09-ibm09n04'(Config) -> run_test(Config, "ibm", "not-wf/P09/ibm09n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n01.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the character "less than".
'ibm-not-wf-P10-ibm10n01'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n02.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the character ampersand.
'ibm-not-wf-P10-ibm10n02'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n03.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the double quote character in the middle.
'ibm-not-wf-P10-ibm10n03'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n04.xml
%% Description:
%%   Tests an attribute with an invalid value. The closing bracket
%%   (double quote) is missing with The value of the attribute "first".
'ibm-not-wf-P10-ibm10n04'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n05.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the character "less than".
'ibm-not-wf-P10-ibm10n05'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n06.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the character ampersand.
'ibm-not-wf-P10-ibm10n06'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n07.xml
%% Description:
%%   Tests an attribute with an invalid value. The value of the attribute
%%   "first" contains the double quote character in the middle.
'ibm-not-wf-P10-ibm10n07'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P10-ibm10n08.xml
%% Description:
%%   Tests an attribute with an invalid value. The closing bracket
%%   (single quote) is missing with the value of the attribute "first".
'ibm-not-wf-P10-ibm10n08'(Config) -> run_test(Config, "ibm", "not-wf/P10/ibm10n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P11-ibm11n01.xml
%% Description:
%%   Tests SystemLiteral. The systemLiteral for the element "student" has
%%   a double quote character in the middle.
'ibm-not-wf-P11-ibm11n01'(Config) -> run_test(Config, "ibm", "not-wf/P11/ibm11n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P11-ibm11n02.xml
%% Description:
%%   Tests SystemLiteral. The systemLiteral for the element "student" has
%%   a single quote character in the middle.
'ibm-not-wf-P11-ibm11n02'(Config) -> run_test(Config, "ibm", "not-wf/P11/ibm11n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P11-ibm11n03.xml
%% Description:
%%   Tests SystemLiteral. The closing bracket (double quote) is missing
%%   with the systemLiteral for the element "student".
'ibm-not-wf-P11-ibm11n03'(Config) -> run_test(Config, "ibm", "not-wf/P11/ibm11n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P11-ibm11n04.xml
%% Description:
%%   Tests SystemLiteral. The closing bracket (single quote) is missing
%%   with the systemLiteral for the element "student".
'ibm-not-wf-P11-ibm11n04'(Config) -> run_test(Config, "ibm", "not-wf/P11/ibm11n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P12-ibm12n01.xml
%% Description:
%%   Tests PubidLiteral. The closing bracket (double quote) is missing
%%   with the value of the PubidLiteral for the entity "info".
'ibm-not-wf-P12-ibm12n01'(Config) -> run_test(Config, "ibm", "not-wf/P12/ibm12n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P12-ibm12n02.xml
%% Description:
%%   Tests PubidLiteral. The value of the PubidLiteral for the entity
%%   "info" has a single quote character in the middle..
'ibm-not-wf-P12-ibm12n02'(Config) -> run_test(Config, "ibm", "not-wf/P12/ibm12n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P12-ibm12n03.xml
%% Description:
%%   Tests PubidLiteral. The closing bracket (single quote) is missing
%%   with the value of the PubidLiteral for the entity "info".
'ibm-not-wf-P12-ibm12n03'(Config) -> run_test(Config, "ibm", "not-wf/P12/ibm12n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P13-ibm13n01.xml
%% Description:
%%   Tests PubidChar. The pubidChar of the PubidLiteral for the entity
%%   "info" contains the character "{".
'ibm-not-wf-P13-ibm13n01'(Config) -> run_test(Config, "ibm", "not-wf/P13/ibm13n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P13-ibm13n02.xml
%% Description:
%%   Tests PubidChar. The pubidChar of the PubidLiteral for the entity
%%   "info" contains the character "~".
'ibm-not-wf-P13-ibm13n02'(Config) -> run_test(Config, "ibm", "not-wf/P13/ibm13n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P13-ibm13n03.xml
%% Description:
%%   Tests PubidChar. The pubidChar of the PubidLiteral for the entity
%%   "info" contains the character double quote in the middle.
'ibm-not-wf-P13-ibm13n03'(Config) -> run_test(Config, "ibm", "not-wf/P13/ibm13n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P14-ibm14n01.xml
%% Description:
%%   Tests CharData. The content of the element "student" contains the
%%   sequence close-bracket close-bracket greater-than.
'ibm-not-wf-P14-ibm14n01'(Config) -> run_test(Config, "ibm", "not-wf/P14/ibm14n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P14-ibm14n02.xml
%% Description:
%%   Tests CharData. The content of the element "student" contains the
%%   character "less than".
'ibm-not-wf-P14-ibm14n02'(Config) -> run_test(Config, "ibm", "not-wf/P14/ibm14n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P14-ibm14n03.xml
%% Description:
%%   Tests CharData. The content of the element "student" contains the
%%   character ampersand.
'ibm-not-wf-P14-ibm14n03'(Config) -> run_test(Config, "ibm", "not-wf/P14/ibm14n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P15-ibm15n01.xml
%% Description:
%%   Tests comment. The text of the second comment contains the character
%%   "-".
'ibm-not-wf-P15-ibm15n01'(Config) -> run_test(Config, "ibm", "not-wf/P15/ibm15n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P15-ibm15n02.xml
%% Description:
%%   Tests comment. The second comment has a wrong closing sequence
%%   "-(greater than)".
'ibm-not-wf-P15-ibm15n02'(Config) -> run_test(Config, "ibm", "not-wf/P15/ibm15n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P15-ibm15n03.xml
%% Description:
%%   Tests comment. The second comment has a wrong beginning sequence
%%   "(less than)!-".
'ibm-not-wf-P15-ibm15n03'(Config) -> run_test(Config, "ibm", "not-wf/P15/ibm15n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P15-ibm15n04.xml
%% Description:
%%   Tests comment. The closing sequence is missing with the second
%%   comment.
'ibm-not-wf-P15-ibm15n04'(Config) -> run_test(Config, "ibm", "not-wf/P15/ibm15n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P16-ibm16n01.xml
%% Description:
%%   Tests PI. The content of the PI includes the sequence "?(greater
%%   than)?".
'ibm-not-wf-P16-ibm16n01'(Config) -> run_test(Config, "ibm", "not-wf/P16/ibm16n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P16-ibm16n02.xml
%% Description:
%%   Tests PI. The PITarget is missing in the PI.
'ibm-not-wf-P16-ibm16n02'(Config) -> run_test(Config, "ibm", "not-wf/P16/ibm16n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P16-ibm16n03.xml
%% Description:
%%   Tests PI. The PI has a wrong closing sequence ">".
'ibm-not-wf-P16-ibm16n03'(Config) -> run_test(Config, "ibm", "not-wf/P16/ibm16n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P16-ibm16n04.xml
%% Description:
%%   Tests PI. The closing sequence is missing in the PI.
'ibm-not-wf-P16-ibm16n04'(Config) -> run_test(Config, "ibm", "not-wf/P16/ibm16n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P17-ibm17n01.xml
%% Description:
%%   Tests PITarget. The PITarget contains the string "XML".
'ibm-not-wf-P17-ibm17n01'(Config) -> run_test(Config, "ibm", "not-wf/P17/ibm17n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P17-ibm17n02.xml
%% Description:
%%   Tests PITarget. The PITarget contains the string "xML".
'ibm-not-wf-P17-ibm17n02'(Config) -> run_test(Config, "ibm", "not-wf/P17/ibm17n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P17-ibm17n03.xml
%% Description:
%%   Tests PITarget. The PITarget contains the string "xml".
'ibm-not-wf-P17-ibm17n03'(Config) -> run_test(Config, "ibm", "not-wf/P17/ibm17n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P17-ibm17n04.xml
%% Description:
%%   Tests PITarget. The PITarget contains the string "xmL".
'ibm-not-wf-P17-ibm17n04'(Config) -> run_test(Config, "ibm", "not-wf/P17/ibm17n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P18-ibm18n01.xml
%% Description:
%%   Tests CDSect. The CDStart is missing in the CDSect in the content of
%%   element "student".
'ibm-not-wf-P18-ibm18n01'(Config) -> run_test(Config, "ibm", "not-wf/P18/ibm18n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P18-ibm18n02.xml
%% Description:
%%   Tests CDSect. The CDEnd is missing in the CDSect in the content of
%%   element "student".
'ibm-not-wf-P18-ibm18n02'(Config) -> run_test(Config, "ibm", "not-wf/P18/ibm18n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P19-ibm19n01.xml
%% Description:
%%   Tests CDStart. The CDStart contains a lower case string "cdata".
'ibm-not-wf-P19-ibm19n01'(Config) -> run_test(Config, "ibm", "not-wf/P19/ibm19n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P19-ibm19n02.xml
%% Description:
%%   Tests CDStart. The CDStart contains an extra character "[".
'ibm-not-wf-P19-ibm19n02'(Config) -> run_test(Config, "ibm", "not-wf/P19/ibm19n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P19-ibm19n03.xml
%% Description:
%%   Tests CDStart. The CDStart contains a wrong character "?".
'ibm-not-wf-P19-ibm19n03'(Config) -> run_test(Config, "ibm", "not-wf/P19/ibm19n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P20-ibm20n01.xml
%% Description:
%%   Tests CDATA with an illegal sequence. The CDATA contains the
%%   sequence close-bracket close-bracket greater-than.
'ibm-not-wf-P20-ibm20n01'(Config) -> run_test(Config, "ibm", "not-wf/P20/ibm20n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P21-ibm21n01.xml
%% Description:
%%   Tests CDEnd. One "]" is missing in the CDEnd.
'ibm-not-wf-P21-ibm21n01'(Config) -> run_test(Config, "ibm", "not-wf/P21/ibm21n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P21-ibm21n02.xml
%% Description:
%%   Tests CDEnd. An extra "]" is placed in the CDEnd.
'ibm-not-wf-P21-ibm21n02'(Config) -> run_test(Config, "ibm", "not-wf/P21/ibm21n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P21-ibm21n03.xml
%% Description:
%%   Tests CDEnd. A wrong character ")" is placed in the CDEnd.
'ibm-not-wf-P21-ibm21n03'(Config) -> run_test(Config, "ibm", "not-wf/P21/ibm21n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P22-ibm22n01.xml
%% Description:
%%   Tests prolog with wrong field ordering. The XMLDecl occurs after the
%%   DTD.
'ibm-not-wf-P22-ibm22n01'(Config) -> run_test(Config, "ibm", "not-wf/P22/ibm22n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P22-ibm22n02.xml
%% Description:
%%   Tests prolog with wrong field ordering. The Misc (comment) occurs
%%   before the XMLDecl.
'ibm-not-wf-P22-ibm22n02'(Config) -> run_test(Config, "ibm", "not-wf/P22/ibm22n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P22-ibm22n03.xml
%% Description:
%%   Tests prolog with wrong field ordering. The XMLDecl occurs after the
%%   DTD and a comment. The other comment occurs before the DTD.
'ibm-not-wf-P22-ibm22n03'(Config) -> run_test(Config, "ibm", "not-wf/P22/ibm22n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n01.xml
%% Description:
%%   Tests XMLDecl with a required field missing. The Versioninfo is
%%   missing in the XMLDecl.
'ibm-not-wf-P23-ibm23n01'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n02.xml
%% Description:
%%   Tests XMLDecl with wrong field ordering. The VersionInfo occurs
%%   after the EncodingDecl.
'ibm-not-wf-P23-ibm23n02'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n03.xml
%% Description:
%%   Tests XMLDecl with wrong field ordering. The VersionInfo occurs
%%   after the SDDecl and the SDDecl occurs after the VersionInfo.
'ibm-not-wf-P23-ibm23n03'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n04.xml
%% Description:
%%   Tests XMLDecl with wrong key word. An upper case string "XML" is
%%   used as the key word in the XMLDecl.
'ibm-not-wf-P23-ibm23n04'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n05.xml
%% Description:
%%   Tests XMLDecl with a wrong closing sequence ">".
'ibm-not-wf-P23-ibm23n05'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P23-ibm23n06.xml
%% Description:
%%   Tests XMLDecl with a wrong opening sequence "(less than)!".
'ibm-not-wf-P23-ibm23n06'(Config) -> run_test(Config, "ibm", "not-wf/P23/ibm23n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n01.xml
%% Description:
%%   Tests VersionInfo with a required field missing. The VersionNum is
%%   missing in the VersionInfo in the XMLDecl.
'ibm-not-wf-P24-ibm24n01'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n02.xml
%% Description:
%%   Tests VersionInfo with a required field missing. The white space is
%%   missing between the key word "xml" and the VersionInfo in the
%%   XMLDecl.
'ibm-not-wf-P24-ibm24n02'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n03.xml
%% Description:
%%   Tests VersionInfo with a required field missing. The "=" (equal
%%   sign) is missing between the key word "version" and the VersionNum.
'ibm-not-wf-P24-ibm24n03'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n04.xml
%% Description:
%%   Tests VersionInfo with wrong field ordering. The VersionNum occurs
%%   before "=" and "version".
'ibm-not-wf-P24-ibm24n04'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n05.xml
%% Description:
%%   Tests VersionInfo with wrong field ordering. The "=" occurs after
%%   "version" and the VersionNum.
'ibm-not-wf-P24-ibm24n05'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n06.xml
%% Description:
%%   Tests VersionInfo with the wrong key word "Version".
'ibm-not-wf-P24-ibm24n06'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n07.xml
%% Description:
%%   Tests VersionInfo with the wrong key word "versioN".
'ibm-not-wf-P24-ibm24n07'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n08.xml
%% Description:
%%   Tests VersionInfo with mismatched quotes around the VersionNum.
%%   version = '1.0" is used as the VersionInfo.
'ibm-not-wf-P24-ibm24n08'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P24-ibm24n09.xml
%% Description:
%%   Tests VersionInfo with mismatched quotes around the VersionNum. The
%%   closing bracket for the VersionNum is missing.
'ibm-not-wf-P24-ibm24n09'(Config) -> run_test(Config, "ibm", "not-wf/P24/ibm24n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P25-ibm25n01.xml
%% Description:
%%   Tests eq with a wrong key word "==".
'ibm-not-wf-P25-ibm25n01'(Config) -> run_test(Config, "ibm", "not-wf/P25/ibm25n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P25-ibm25n02.xml
%% Description:
%%   Tests eq with a wrong key word "eq".
'ibm-not-wf-P25-ibm25n02'(Config) -> run_test(Config, "ibm", "not-wf/P25/ibm25n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P26-ibm26n01.xml
%% Description:
%%   Tests VersionNum with an illegal character "#".
'ibm-not-wf-P26-ibm26n01'(Config) -> run_test(Config, "ibm", "not-wf/P26/ibm26n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P27-ibm27n01.xml
%% Description:
%%   Tests type of Misc. An element declaration is used as a type of Misc
%%   After the element "animal".
'ibm-not-wf-P27-ibm27n01'(Config) -> run_test(Config, "ibm", "not-wf/P27/ibm27n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n01.xml
%% Description:
%%   Tests doctypedecl with a required field missing. The Name "animal"
%%   is missing in the doctypedecl.
'ibm-not-wf-P28-ibm28n01'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n02.xml
%% Description:
%%   Tests doctypedecl with wrong field ordering. The Name "animal"
%%   occurs after the markup declarations inside the "[]".
'ibm-not-wf-P28-ibm28n02'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n03.xml
%% Description:
%%   Tests doctypedecl with wrong field ordering. The Name "animal"
%%   occurs after the markup declarations inside the "[]".
'ibm-not-wf-P28-ibm28n03'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n04.xml
%% Description:
%%   Tests doctypedecl with general entity reference.The
%%   "(ampersand)generalE" occurs in the DTD.
'ibm-not-wf-P28-ibm28n04'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n05.xml
%% Description:
%%   Tests doctypedecl with wrong key word. A wrong key word "DOCtYPE"
%%   occurs on line 2.
'ibm-not-wf-P28-ibm28n05'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n06.xml
%% Description:
%%   Tests doctypedecl with mismatched brackets. The closing bracket "]"
%%   of the DTD is missing.
'ibm-not-wf-P28-ibm28n06'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n07.xml
%% Description:
%%   Tests doctypedecl with wrong bracket. The opening bracket "{" occurs
%%   in the DTD.
'ibm-not-wf-P28-ibm28n07'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P28-ibm28n08.xml
%% Description:
%%   Tests doctypedecl with wrong opening sequence. The opening sequence
%%   "(less than)?DOCTYPE" occurs in the DTD.
'ibm-not-wf-P28-ibm28n08'(Config) -> run_test(Config, "ibm", "not-wf/P28/ibm28n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-p28a-ibm28an01.xml
%% Entities: parameter
%% Description:
%%   This test violates WFC:PE Between Declarations in Production 28a.
%%   The last character of a markup declaration is not contained in the
%%   same parameter-entity text replacement.

%% run_test(Config, "ibm", "not-wf/p28a/ibm28an01.xml", "not-wf").
'ibm-not-wf-p28a-ibm28an01'(_Config) ->
    {skip, "ISSUE: Violates WFC:PE Between Declarations in Production 28a"}.

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n01.xml
%% Description:
%%   Tests markupdecl with an illegal markup declaration. A XMLDecl
%%   occurs inside the DTD.
'ibm-not-wf-P29-ibm29n01'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n02.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside an
%%   elementdecl in the DTD.
'ibm-not-wf-P29-ibm29n02'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n03.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside an
%%   ATTlistDecl in the DTD.
'ibm-not-wf-P29-ibm29n03'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n04.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside an
%%   EntityDecl in the DTD.
'ibm-not-wf-P29-ibm29n04'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n05.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside a
%%   PI in the DTD.
'ibm-not-wf-P29-ibm29n05'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n06.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside a
%%   comment in the DTD.
'ibm-not-wf-P29-ibm29n06'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P29-ibm29n07.xml
%% Description:
%%   Tests WFC "PEs in Internal Subset". A PE reference occurs inside a
%%   NotationDecl in the DTD.
'ibm-not-wf-P29-ibm29n07'(Config) -> run_test(Config, "ibm", "not-wf/P29/ibm29n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P30-ibm30n01.xml
%% Entities: parameter
%% Description:
%%   Tests extSubset with wrong field ordering. In the file
%%   "ibm30n01.dtd", the TextDecl occurs after the extSubsetDecl (the
%%   element declaration).
'ibm-not-wf-P30-ibm30n01'(Config) -> run_test(Config, "ibm", "not-wf/P30/ibm30n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P31-ibm31n01.xml
%% Entities: parameter
%% Description:
%%   Tests extSubsetDecl with an illegal field. A general entity
%%   reference occurs in file "ibm31n01.dtd".
'ibm-not-wf-P31-ibm31n01'(Config) -> run_test(Config, "ibm", "not-wf/P31/ibm31n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n01.xml
%% Description:
%%   Tests SDDecl with a required field missing. The leading white space
%%   is missing with the SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n01'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n02.xml
%% Description:
%%   Tests SDDecl with a required field missing. The "=" sign is missing
%%   in the SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n02'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n03.xml
%% Description:
%%   Tests SDDecl with wrong key word. The word "Standalone" occurs in
%%   the SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n03'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n04.xml
%% Description:
%%   Tests SDDecl with wrong key word. The word "Yes" occurs in the
%%   SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n04'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n05.xml
%% Description:
%%   Tests SDDecl with wrong key word. The word "YES" occurs in the
%%   SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n05'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n06.xml
%% Description:
%%   Tests SDDecl with wrong key word. The word "No" occurs in the SDDecl
%%   in the XMLDecl.
'ibm-not-wf-P32-ibm32n06'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n07.xml
%% Description:
%%   Tests SDDecl with wrong key word. The word "NO" occurs in the SDDecl
%%   in the XMLDecl.
'ibm-not-wf-P32-ibm32n07'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n08.xml
%% Description:
%%   Tests SDDecl with wrong field ordering. The "=" sign occurs after
%%   the key word "yes" in the SDDecl in the XMLDecl.
'ibm-not-wf-P32-ibm32n08'(Config) -> run_test(Config, "ibm", "not-wf/P32/ibm32n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P32-ibm32n09.xml
%% Entities: parameter
%% Description:
%%   This is test violates WFC: Entity Declared in P68. The standalone
%%   document declaration has the value yes, BUT there is an external
%%   markup declaration of an entity (other than amp, lt, gt, apos,
%%   quot), and references to this entity appear in the document.

%% run_test(Config, "ibm", "not-wf/P32/ibm32n09.xml", "not-wf").
'ibm-not-wf-P32-ibm32n09'(_Config) -> {skip, "ISSUE: Violates standalone=yes"}.

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n01.xml
%% Description:
%%   Tests element with a required field missing. The ETag is missing for
%%   the element "root".
'ibm-not-wf-P39-ibm39n01'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n02.xml
%% Description:
%%   Tests element with a required field missing. The STag is missing for
%%   the element "root".
'ibm-not-wf-P39-ibm39n02'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n03.xml
%% Description:
%%   Tests element with required fields missing. Both the content and the
%%   ETag are missing in the element "root".
'ibm-not-wf-P39-ibm39n03'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n04.xml
%% Description:
%%   Tests element with required fields missing. Both the content and the
%%   STag are missing in the element "root".
'ibm-not-wf-P39-ibm39n04'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n05.xml
%% Description:
%%   Tests element with wrong field ordering. The STag and the ETag are
%%   swapped in the element "root".
'ibm-not-wf-P39-ibm39n05'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P39-ibm39n06.xml
%% Description:
%%   Tests element with wrong field ordering. The content occurs after
%%   the ETag of the element "root".
'ibm-not-wf-P39-ibm39n06'(Config) -> run_test(Config, "ibm", "not-wf/P39/ibm39n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P40-ibm40n01.xml
%% Description:
%%   Tests STag with a required field missing. The Name "root" is in the
%%   STag of the element "root".
'ibm-not-wf-P40-ibm40n01'(Config) -> run_test(Config, "ibm", "not-wf/P40/ibm40n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P40-ibm40n02.xml
%% Description:
%%   Tests STag with a required field missing. The white space between
%%   the Name "root" and the attribute "attr1" is missing in the STag of
%%   the element "root".
'ibm-not-wf-P40-ibm40n02'(Config) -> run_test(Config, "ibm", "not-wf/P40/ibm40n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P40-ibm40n03.xml
%% Description:
%%   Tests STag with wrong field ordering. The Name "root" occurs after
%%   the attribute "attr1" in the STag of the element "root".
'ibm-not-wf-P40-ibm40n03'(Config) -> run_test(Config, "ibm", "not-wf/P40/ibm40n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P40-ibm40n04.xml
%% Description:
%%   Tests STag with a wrong opening sequence. The string "(less than)!"
%%   is used as the opening sequence for the STag of the element "root".
'ibm-not-wf-P40-ibm40n04'(Config) -> run_test(Config, "ibm", "not-wf/P40/ibm40n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P40-ibm40n05.xml
%% Description:
%%   Tests STag with duplicate attribute names. The attribute name
%%   "attr1" occurs twice in the STag of the element "root".
'ibm-not-wf-P40-ibm40n05'(Config) -> run_test(Config, "ibm", "not-wf/P40/ibm40n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n01.xml
%% Description:
%%   Tests Attribute with a required field missing. The attribute name is
%%   missing in the Attribute in the STag of the element "root".
'ibm-not-wf-P41-ibm41n01'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n02.xml
%% Description:
%%   Tests Attribute with a required field missing. The "=" is missing
%%   between the attribute name and the attribute value in the Attribute
%%   in the STag of the element "root".
'ibm-not-wf-P41-ibm41n02'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n03.xml
%% Description:
%%   Tests Attribute with a required field missing. The AttValue is
%%   missing in the Attribute in the STag of the element "root".
'ibm-not-wf-P41-ibm41n03'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n04.xml
%% Description:
%%   Tests Attribute with a required field missing. The Name and the "="
%%   are missing in the Attribute in the STag of the element "root".
'ibm-not-wf-P41-ibm41n04'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n05.xml
%% Description:
%%   Tests Attribute with a required field missing. The "=" and the
%%   AttValue are missing in the Attribute in the STag of the element
%%   "root".
'ibm-not-wf-P41-ibm41n05'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n06.xml
%% Description:
%%   Tests Attribute with a required field missing. The Name and the
%%   AttValue are missing in the Attribute in the STag of the element
%%   "root".
'ibm-not-wf-P41-ibm41n06'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n07.xml
%% Description:
%%   Tests Attribute with wrong field ordering. The "=" occurs after the
%%   Name and the AttValue in the Attribute in the STag of the element
%%   "root".
'ibm-not-wf-P41-ibm41n07'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n08.xml
%% Description:
%%   Tests Attribute with wrong field ordering. The Name and the AttValue
%%   are swapped in the Attribute in the STag of the element "root".
'ibm-not-wf-P41-ibm41n08'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n09.xml
%% Description:
%%   Tests Attribute with wrong field ordering. The "=" occurs before the
%%   Name and the AttValue in the Attribute in the STag of the element
%%   "root".
'ibm-not-wf-P41-ibm41n09'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n10.xml
%% Description:
%%   Tests Attribute against WFC "no external entity references". A
%%   direct reference to the external entity "aExternal" is contained in
%%   the value of the attribute "attr1".
'ibm-not-wf-P41-ibm41n10'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n11.xml
%% Description:
%%   Tests Attribute against WFC "no external entity references". A
%%   indirect reference to the external entity "aExternal" is contained
%%   in the value of the attribute "attr1".
'ibm-not-wf-P41-ibm41n11'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n12.xml
%% Description:
%%   Tests Attribute against WFC "no external entity references". A
%%   direct reference to the external unparsed entity "aImage" is
%%   contained in the value of the attribute "attr1".
'ibm-not-wf-P41-ibm41n12'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n13.xml
%% Description:
%%   Tests Attribute against WFC "No (less than) character in Attribute
%%   Values". The character "less than" is contained in the value of the
%%   attribute "attr1".
'ibm-not-wf-P41-ibm41n13'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P41-ibm41n14.xml
%% Description:
%%   Tests Attribute against WFC "No (less than) in Attribute Values".
%%   The character "less than" is contained in the value of the attribute
%%   "attr1" through indirect internal entity reference.
'ibm-not-wf-P41-ibm41n14'(Config) -> run_test(Config, "ibm", "not-wf/P41/ibm41n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P42-ibm42n01.xml
%% Description:
%%   Tests ETag with a required field missing. The Name is missing in the
%%   ETag of the element "root".
'ibm-not-wf-P42-ibm42n01'(Config) -> run_test(Config, "ibm", "not-wf/P42/ibm42n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P42-ibm42n02.xml
%% Description:
%%   Tests ETag with a wrong beginning sequence. The string "(less
%%   than)\" is used as a beginning sequence of the ETag of the element
%%   "root".
'ibm-not-wf-P42-ibm42n02'(Config) -> run_test(Config, "ibm", "not-wf/P42/ibm42n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P42-ibm42n03.xml
%% Description:
%%   Tests ETag with a wrong beginning sequence. The string "less than"
%%   is used as a beginning sequence of the ETag of the element "root".
'ibm-not-wf-P42-ibm42n03'(Config) -> run_test(Config, "ibm", "not-wf/P42/ibm42n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P42-ibm42n04.xml
%% Description:
%%   Tests ETag with a wrong structure. An white space occurs between The
%%   beginning sequence and the Name of the ETag of the element "root".
'ibm-not-wf-P42-ibm42n04'(Config) -> run_test(Config, "ibm", "not-wf/P42/ibm42n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P42-ibm42n05.xml
%% Description:
%%   Tests ETag with a wrong structure. The ETag of the element "root"
%%   contains an Attribute (attr1="any").
'ibm-not-wf-P42-ibm42n05'(Config) -> run_test(Config, "ibm", "not-wf/P42/ibm42n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P43-ibm43n01.xml
%% Description:
%%   Tests element content with a wrong option. A NotationDecl is used as
%%   the content of the element "root".
'ibm-not-wf-P43-ibm43n01'(Config) -> run_test(Config, "ibm", "not-wf/P43/ibm43n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P43-ibm43n02.xml
%% Description:
%%   Tests element content with a wrong option. An elementdecl is used as
%%   the content of the element "root".
'ibm-not-wf-P43-ibm43n02'(Config) -> run_test(Config, "ibm", "not-wf/P43/ibm43n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P43-ibm43n04.xml
%% Description:
%%   Tests element content with a wrong option. An entitydecl is used as
%%   the content of the element "root".
'ibm-not-wf-P43-ibm43n04'(Config) -> run_test(Config, "ibm", "not-wf/P43/ibm43n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P43-ibm43n05.xml
%% Description:
%%   Tests element content with a wrong option. An AttlistDecl is used as
%%   the content of the element "root".
'ibm-not-wf-P43-ibm43n05'(Config) -> run_test(Config, "ibm", "not-wf/P43/ibm43n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P44-ibm44n01.xml
%% Description:
%%   Tests EmptyElemTag with a required field missing. The Name "root" is
%%   missing in the EmptyElemTag.
'ibm-not-wf-P44-ibm44n01'(Config) -> run_test(Config, "ibm", "not-wf/P44/ibm44n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P44-ibm44n02.xml
%% Description:
%%   Tests EmptyElemTag with wrong field ordering. The Attribute (attri1
%%   = "any") occurs before the name of the element "root" in the
%%   EmptyElemTag.
'ibm-not-wf-P44-ibm44n02'(Config) -> run_test(Config, "ibm", "not-wf/P44/ibm44n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P44-ibm44n03.xml
%% Description:
%%   Tests EmptyElemTag with wrong closing sequence. The string "\>" is
%%   used as the closing sequence in the EmptyElemtag of the element
%%   "root".
'ibm-not-wf-P44-ibm44n03'(Config) -> run_test(Config, "ibm", "not-wf/P44/ibm44n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P44-ibm44n04.xml
%% Description:
%%   Tests EmptyElemTag which against the WFC "Unique Att Spec". The
%%   attribute name "attr1" occurs twice in the EmptyElemTag of the
%%   element "root".
'ibm-not-wf-P44-ibm44n04'(Config) -> run_test(Config, "ibm", "not-wf/P44/ibm44n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n01.xml
%% Description:
%%   Tests elementdecl with a required field missing. The Name is missing
%%   in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n01'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n02.xml
%% Description:
%%   Tests elementdecl with a required field missing. The white space is
%%   missing between "aEle" and "(#PCDATA)" in the second elementdecl in
%%   the DTD.
'ibm-not-wf-P45-ibm45n02'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n03.xml
%% Description:
%%   Tests elementdecl with a required field missing. The contentspec is
%%   missing in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n03'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n04.xml
%% Description:
%%   Tests elementdecl with a required field missing. The contentspec and
%%   the white space is missing in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n04'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n05.xml
%% Description:
%%   Tests elementdecl with a required field missing. The Name, the white
%%   space, and the contentspec are missing in the second elementdecl in
%%   the DTD.
'ibm-not-wf-P45-ibm45n05'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n06.xml
%% Description:
%%   Tests elementdecl with wrong field ordering. The Name occurs after
%%   the contentspec in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n06'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n07.xml
%% Description:
%%   Tests elementdecl with wrong beginning sequence. The string "(less
%%   than)ELEMENT" is used as the beginning sequence in the second
%%   elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n07'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n08.xml
%% Description:
%%   Tests elementdecl with wrong key word. The string "Element" is used
%%   as the key word in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n08'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P45-ibm45n09.xml
%% Description:
%%   Tests elementdecl with wrong key word. The string "element" is used
%%   as the key word in the second elementdecl in the DTD.
'ibm-not-wf-P45-ibm45n09'(Config) -> run_test(Config, "ibm", "not-wf/P45/ibm45n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P46-ibm46n01.xml
%% Description:
%%   Tests contentspec with wrong key word. the string "empty" is used as
%%   the key word in the contentspec of the second elementdecl in the
%%   DTD.
'ibm-not-wf-P46-ibm46n01'(Config) -> run_test(Config, "ibm", "not-wf/P46/ibm46n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P46-ibm46n02.xml
%% Description:
%%   Tests contentspec with wrong key word. the string "Empty" is used as
%%   the key word in the contentspec of the second elementdecl in the
%%   DTD.
'ibm-not-wf-P46-ibm46n02'(Config) -> run_test(Config, "ibm", "not-wf/P46/ibm46n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P46-ibm46n03.xml
%% Description:
%%   Tests contentspec with wrong key word. the string "Any" is used as
%%   the key word in the contentspec of the second elementdecl in the
%%   DTD.
'ibm-not-wf-P46-ibm46n03'(Config) -> run_test(Config, "ibm", "not-wf/P46/ibm46n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P46-ibm46n04.xml
%% Description:
%%   Tests contentspec with wrong key word. the string "any" is used as
%%   the key word in the contentspec of the second elementdecl in the
%%   DTD.
'ibm-not-wf-P46-ibm46n04'(Config) -> run_test(Config, "ibm", "not-wf/P46/ibm46n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P46-ibm46n05.xml
%% Description:
%%   Tests contentspec with a wrong option. The string "#CDATA" is used
%%   as the contentspec in the second elementdecl in the DTD.
'ibm-not-wf-P46-ibm46n05'(Config) -> run_test(Config, "ibm", "not-wf/P46/ibm46n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n01.xml
%% Description:
%%   Tests children with a required field missing. The "+" is used as the
%%   choice or seq field in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n01'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n02.xml
%% Description:
%%   Tests children with a required field missing. The "*" is used as the
%%   choice or seq field in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n02'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n03.xml
%% Description:
%%   Tests children with a required field missing. The "?" is used as the
%%   choice or seq field in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n03'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n04.xml
%% Description:
%%   Tests children with wrong field ordering. The "*" occurs before the
%%   seq field (a,a) in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n04'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n05.xml
%% Description:
%%   Tests children with wrong field ordering. The "+" occurs before the
%%   choice field (a|a) in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n05'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P47-ibm47n06.xml
%% Description:
%%   Tests children with wrong key word. The "^" occurs after the seq
%%   field in the second elementdecl in the DTD.
'ibm-not-wf-P47-ibm47n06'(Config) -> run_test(Config, "ibm", "not-wf/P47/ibm47n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n01.xml
%% Description:
%%   Tests cp with a required fields missing. The field Name|choice|seq
%%   is missing in the second cp in the choice field in the third
%%   elementdecl in the DTD.
'ibm-not-wf-P48-ibm48n01'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n02.xml
%% Description:
%%   Tests cp with a required fields missing. The field Name|choice|seq
%%   is missing in the cp in the third elementdecl in the DTD.
'ibm-not-wf-P48-ibm48n02'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n03.xml
%% Description:
%%   Tests cp with a required fields missing. The field Name|choice|seq
%%   is missing in the first cp in the choice field in the third
%%   elementdecl in the DTD.
'ibm-not-wf-P48-ibm48n03'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n04.xml
%% Description:
%%   Tests cp with wrong field ordering. The "+" occurs before the seq
%%   (a,a) in the first cp in the choice field in the third elementdecl
%%   in the DTD.
'ibm-not-wf-P48-ibm48n04'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n05.xml
%% Description:
%%   Tests cp with wrong field ordering. The "*" occurs before the choice
%%   (a|b) in the first cp in the seq field in the third elementdecl in
%%   the DTD.
'ibm-not-wf-P48-ibm48n05'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n06.xml
%% Description:
%%   Tests cp with wrong field ordering. The "?" occurs before the Name
%%   "a" in the second cp in the seq field in the third elementdecl in
%%   the DTD.
'ibm-not-wf-P48-ibm48n06'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P48-ibm48n07.xml
%% Description:
%%   Tests cp with wrong key word. The "^" occurs after the Name "a" in
%%   the first cp in the choice field in the third elementdecl in the
%%   DTD.
'ibm-not-wf-P48-ibm48n07'(Config) -> run_test(Config, "ibm", "not-wf/P48/ibm48n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n01.xml
%% Description:
%%   Tests choice with a required field missing. The two cps are missing
%%   in the choice field in the third elementdecl in the DTD.
'ibm-not-wf-P49-ibm49n01'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n02.xml
%% Description:
%%   Tests choice with a required field missing. The third cp is missing
%%   in the choice field in the fourth elementdecl in the DTD.
'ibm-not-wf-P49-ibm49n02'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n03.xml
%% Description:
%%   Tests choice with a wrong separator. The "!" is used as the
%%   separator in the choice field in the fourth elementdecl in the DTD.
'ibm-not-wf-P49-ibm49n03'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n04.xml
%% Description:
%%   Tests choice with a required field missing. The separator "|" is
%%   missing in the choice field (a b)+ in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P49-ibm49n04'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n05.xml
%% Description:
%%   Tests choice with an extra separator. An extra "|" occurs between a
%%   and b in the choice field in the fourth elementdecl in the DTD.
'ibm-not-wf-P49-ibm49n05'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P49-ibm49n06.xml
%% Description:
%%   Tests choice with a required field missing. The closing bracket ")"
%%   is missing in the choice field (a |b * in the fourth elementdecl in
%%   the DTD.
'ibm-not-wf-P49-ibm49n06'(Config) -> run_test(Config, "ibm", "not-wf/P49/ibm49n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n01.xml
%% Description:
%%   Tests seq with a required field missing. The two cps are missing in
%%   the seq field in the fourth elementdecl in the DTD.
'ibm-not-wf-P50-ibm50n01'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n02.xml
%% Description:
%%   Tests seq with a required field missing. The third cp is missing in
%%   the seq field in the fourth elementdecl in the DTD.
'ibm-not-wf-P50-ibm50n02'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n03.xml
%% Description:
%%   Tests seq with a wrong separator. The "|" is used as the separator
%%   between a and b in the seq field in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P50-ibm50n03'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n04.xml
%% Description:
%%   Tests seq with a wrong separator. The "." is used as the separator
%%   between a and b in the seq field in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P50-ibm50n04'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n05.xml
%% Description:
%%   Tests seq with an extra separator. An extra "," occurs between (a|b)
%%   and a in the seq field in the fourth elementdecl in the DTD.
'ibm-not-wf-P50-ibm50n05'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n06.xml
%% Description:
%%   Tests seq with a required field missing. The separator between (a|b)
%%   and (b|a) is missing in the seq field in the fourth elementdecl in
%%   the DTD.
'ibm-not-wf-P50-ibm50n06'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P50-ibm50n07.xml
%% Description:
%%   Tests seq with wrong closing bracket. The "]" is used as the closing
%%   bracket in the seq field in the fourth elementdecl in the DTD.
'ibm-not-wf-P50-ibm50n07'(Config) -> run_test(Config, "ibm", "not-wf/P50/ibm50n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n01.xml
%% Description:
%%   Tests Mixed with a wrong key word. The string "#pcdata" is used as
%%   the key word in the Mixed field in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P51-ibm51n01'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n02.xml
%% Description:
%%   Tests Mixed with wrong field ordering. The field #PCDATA does not
%%   occur as the first component in the Mixed field in the fourth
%%   elementdecl in the DTD.
'ibm-not-wf-P51-ibm51n02'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n03.xml
%% Description:
%%   Tests Mixed with a separator missing. The separator "|" is missing
%%   in between #PCDATA and a in the Mixed field in the fourth
%%   elementdecl in the DTD.
'ibm-not-wf-P51-ibm51n03'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n04.xml
%% Description:
%%   Tests Mixed with a wrong key word. The string "#CDATA" is used as
%%   the key word in the Mixed field in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P51-ibm51n04'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n05.xml
%% Description:
%%   Tests Mixed with a required field missing. The "*" is missing after
%%   the ")" in the Mixed field in the fourth elementdecl in the DTD.
'ibm-not-wf-P51-ibm51n05'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n06.xml
%% Description:
%%   Tests Mixed with wrong closing bracket. The "]" is used as the
%%   closing bracket in the Mixed field in the fourth elementdecl in the
%%   DTD.
'ibm-not-wf-P51-ibm51n06'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P51-ibm51n07.xml
%% Description:
%%   Tests Mixed with a required field missing. The closing bracket ")"
%%   is missing after (#PCDATA in the Mixed field in the fourth
%%   elementdecl in the DTD.
'ibm-not-wf-P51-ibm51n07'(Config) -> run_test(Config, "ibm", "not-wf/P51/ibm51n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n01.xml
%% Description:
%%   Tests AttlistDecl with a required field missing. The Name is missing
%%   in the AttlistDecl in the DTD.
'ibm-not-wf-P52-ibm52n01'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n02.xml
%% Description:
%%   Tests AttlistDecl with a required field missing. The white space is
%%   missing between the beginning sequence and the name in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P52-ibm52n02'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n03.xml
%% Description:
%%   Tests AttlistDecl with wrong field ordering. The Name "a" occurs
%%   after the first AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P52-ibm52n03'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n04.xml
%% Description:
%%   Tests AttlistDecl with wrong key word. The string "Attlist" is used
%%   as the key word in the beginning sequence in the AttlistDecl in the
%%   DTD.
'ibm-not-wf-P52-ibm52n04'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n05.xml
%% Description:
%%   Tests AttlistDecl with a required field missing. The closing bracket
%%   "greater than" is missing in the AttlistDecl in the DTD.
'ibm-not-wf-P52-ibm52n05'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P52-ibm52n06.xml
%% Description:
%%   Tests AttlistDecl with wrong beginning sequence. The string "(less
%%   than)ATTLIST" is used as the beginning sequence in the AttlistDecl
%%   in the DTD.
'ibm-not-wf-P52-ibm52n06'(Config) -> run_test(Config, "ibm", "not-wf/P52/ibm52n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n01.xml
%% Description:
%%   Tests AttDef with a required field missing. The DefaultDecl is
%%   missing in the AttDef for the name "attr1" in the AttlistDecl in the
%%   DTD.
'ibm-not-wf-P53-ibm53n01'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n02.xml
%% Description:
%%   Tests AttDef with a required field missing. The white space is
%%   missing between (abc|def) and "def" in the AttDef in the AttlistDecl
%%   in the DTD.
'ibm-not-wf-P53-ibm53n02'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n03.xml
%% Description:
%%   Tests AttDef with a required field missing. The AttType is missing
%%   for "attr1" in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P53-ibm53n03'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n04.xml
%% Description:
%%   Tests AttDef with a required field missing. The white space is
%%   missing between "attr1" and (abc|def) in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P53-ibm53n04'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n05.xml
%% Description:
%%   Tests AttDef with a required field missing. The Name is missing in
%%   the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P53-ibm53n05'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n06.xml
%% Description:
%%   Tests AttDef with a required field missing. The white space before
%%   the name "attr2" is missing in the AttDef in the AttlistDecl in the
%%   DTD.
'ibm-not-wf-P53-ibm53n06'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n07.xml
%% Description:
%%   Tests AttDef with wrong field ordering. The Name "attr1" occurs
%%   after the AttType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P53-ibm53n07'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P53-ibm53n08.xml
%% Description:
%%   Tests AttDef with wrong field ordering. The Name "attr1" occurs
%%   after the AttType and "default" occurs before the AttType in the
%%   AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P53-ibm53n08'(Config) -> run_test(Config, "ibm", "not-wf/P53/ibm53n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P54-ibm54n01.xml
%% Description:
%%   Tests AttType with a wrong option. The string "BOGUSATTR" is used as
%%   the AttType in the AttlistDecl in the DTD.
'ibm-not-wf-P54-ibm54n01'(Config) -> run_test(Config, "ibm", "not-wf/P54/ibm54n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P54-ibm54n02.xml
%% Description:
%%   Tests AttType with a wrong option. The string "PCDATA" is used as
%%   the AttType in the AttlistDecl in the DTD.
'ibm-not-wf-P54-ibm54n02'(Config) -> run_test(Config, "ibm", "not-wf/P54/ibm54n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P55-ibm55n01.xml
%% Description:
%%   Tests StringType with a wrong key word. The lower case string
%%   "cdata" is used as the StringType in the AttType in the AttlistDecl
%%   in the DTD.
'ibm-not-wf-P55-ibm55n01'(Config) -> run_test(Config, "ibm", "not-wf/P55/ibm55n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P55-ibm55n02.xml
%% Description:
%%   Tests StringType with a wrong key word. The string "#CDATA" is used
%%   as the StringType in the AttType in the AttlistDecl in the DTD.
'ibm-not-wf-P55-ibm55n02'(Config) -> run_test(Config, "ibm", "not-wf/P55/ibm55n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P55-ibm55n03.xml
%% Description:
%%   Tests StringType with a wrong key word. The string "CData" is used
%%   as the StringType in the AttType in the AttlistDecl in the DTD.
'ibm-not-wf-P55-ibm55n03'(Config) -> run_test(Config, "ibm", "not-wf/P55/ibm55n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n01.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "id" is used in
%%   the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n01'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n02.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "Idref" is used in
%%   the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n02'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n03.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type"Idrefs" is used in
%%   the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n03'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n04.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "EntitY" is used
%%   in the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n04'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n05.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "nmTOKEN" is used
%%   in the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n05'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n06.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "NMtokens" is used
%%   in the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n06'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P56-ibm56n07.xml
%% Description:
%%   Tests TokenizedType with wrong key word. The type "#ID" is used in
%%   the TokenizedType in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P56-ibm56n07'(Config) -> run_test(Config, "ibm", "not-wf/P56/ibm56n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P57-ibm57n01.xml
%% Description:
%%   Tests EnumeratedType with an illegal option. The string "NMTOKEN
%%   (a|b)" is used in the EnumeratedType in the AttlistDecl in the DTD.
'ibm-not-wf-P57-ibm57n01'(Config) -> run_test(Config, "ibm", "not-wf/P57/ibm57n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n01.xml
%% Description:
%%   Tests NotationType with wrong key word. The lower case "notation" is
%%   used as the key word in the NotationType in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n01'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n02.xml
%% Description:
%%   Tests NotationType with a required field missing. The beginning
%%   bracket "(" is missing in the NotationType in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n02'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n03.xml
%% Description:
%%   Tests NotationType with a required field missing. The Name is
%%   missing in the "()" in the NotationType in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n03'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n04.xml
%% Description:
%%   Tests NotationType with a required field missing. The closing
%%   bracket is missing in the NotationType in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n04'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n05.xml
%% Description:
%%   Tests NotationType with wrong field ordering. The key word
%%   "NOTATION" occurs after "(this)" in the NotationType in the AttDef
%%   in the AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n05'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n06.xml
%% Description:
%%   Tests NotationType with wrong separator. The "," is used as a
%%   separator between "this" and "that" in the NotationType in the
%%   AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n06'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n07.xml
%% Description:
%%   Tests NotationType with a required field missing. The white space is
%%   missing between "NOTATION" and "(this)" in the NotationType in the
%%   AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n07'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P58-ibm58n08.xml
%% Description:
%%   Tests NotationType with extra wrong characters. The double quote
%%   character occurs after "(" and before ")" in the NotationType in the
%%   AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P58-ibm58n08'(Config) -> run_test(Config, "ibm", "not-wf/P58/ibm58n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n01.xml
%% Description:
%%   Tests Enumeration with required fields missing. The Nmtokens and
%%   "|"s are missing in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P59-ibm59n01'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n02.xml
%% Description:
%%   Tests Enumeration with a required field missing. The closing bracket
%%   ")" is missing in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P59-ibm59n02'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n03.xml
%% Description:
%%   Tests Enumeration with wrong separator. The "," is used as the
%%   separator in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P59-ibm59n03'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n04.xml
%% Description:
%%   Tests Enumeration with illegal presence. The double quotes occur
%%   around the Enumeration value in the AttDef in the AttlistDecl in the
%%   DTD.
'ibm-not-wf-P59-ibm59n04'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n05.xml
%% Description:
%%   Tests Enumeration with a required field missing. The white space is
%%   missing between in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P59-ibm59n05'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P59-ibm59n06.xml
%% Description:
%%   Tests Enumeration with a required field missing. The beginning
%%   bracket "(" is missing in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P59-ibm59n06'(Config) -> run_test(Config, "ibm", "not-wf/P59/ibm59n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n01.xml
%% Description:
%%   Tests DefaultDecl with wrong key word. The string "#required" is
%%   used as the key word in the DefaultDecl in the AttDef in the
%%   AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n01'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n02.xml
%% Description:
%%   Tests DefaultDecl with wrong key word. The string "Implied" is used
%%   as the key word in the DefaultDecl in the AttDef in the AttlistDecl
%%   in the DTD.
'ibm-not-wf-P60-ibm60n02'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n03.xml
%% Description:
%%   Tests DefaultDecl with wrong key word. The string "!IMPLIED" is used
%%   as the key word in the DefaultDecl in the AttDef in the AttlistDecl
%%   in the DTD.
'ibm-not-wf-P60-ibm60n03'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n04.xml
%% Description:
%%   Tests DefaultDecl with a required field missing. There is no
%%   attribute value specified after the key word "#FIXED" in the
%%   DefaultDecl in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n04'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n05.xml
%% Description:
%%   Tests DefaultDecl with a required field missing. The white space is
%%   missing between the key word "#FIXED" and the attribute value in the
%%   DefaultDecl in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n05'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n06.xml
%% Description:
%%   Tests DefaultDecl with wrong field ordering. The key word "#FIXED"
%%   occurs after the attribute value "introduction" in the DefaultDecl
%%   in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n06'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n07.xml
%% Description:
%%   Tests DefaultDecl against WFC of P60. The text replacement of the
%%   entity "avalue" contains the "less than" character in the
%%   DefaultDecl in the AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n07'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P60-ibm60n08.xml
%% Description:
%%   Tests DefaultDecl with more than one key word. The "#REQUIRED" and
%%   the "#IMPLIED" are used as the key words in the DefaultDecl in the
%%   AttDef in the AttlistDecl in the DTD.
'ibm-not-wf-P60-ibm60n08'(Config) -> run_test(Config, "ibm", "not-wf/P60/ibm60n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P61-ibm61n01.xml
%% Entities: parameter
%% Description:
%%   Tests conditionalSect with a wrong option. The word "NOTINCLUDE" is
%%   used as part of an option which is wrong in the coditionalSect.
'ibm-not-wf-P61-ibm61n01'(Config) -> run_test(Config, "ibm", "not-wf/P61/ibm61n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n01.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with wrong key word. The string "include" is used
%%   as a key word in the beginning sequence in the includeSect in the
%%   file ibm62n01.dtd.
'ibm-not-wf-P62-ibm62n01'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n02.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with wrong beginning sequence. An extra "[" occurs
%%   in the beginning sequence in the includeSect in the file
%%   ibm62n02.dtd.
'ibm-not-wf-P62-ibm62n02'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n03.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with wrong beginning sequence. A wrong character
%%   "?" occurs in the beginning sequence in the includeSect in the file
%%   ibm62n03.dtd.
'ibm-not-wf-P62-ibm62n03'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n04.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with a required field missing. The key word
%%   "INCLUDE" is missing in the includeSect in the file ibm62n04.dtd.
'ibm-not-wf-P62-ibm62n04'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n05.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with a required field missing. The "[" is missing
%%   after the key word "INCLUDE" in the includeSect in the file
%%   ibm62n05.dtd.
'ibm-not-wf-P62-ibm62n05'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n06.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with wrong field ordering. The two external subset
%%   declarations occur before the key word "INCLUDE" in the includeSect
%%   in the file ibm62n06.dtd.
'ibm-not-wf-P62-ibm62n06'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n07.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with a required field missing. The closing
%%   sequence "]](greater than)" is missing in the includeSect in the
%%   file ibm62n07.dtd.
'ibm-not-wf-P62-ibm62n07'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P62-ibm62n08.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with a required field missing. One "]" is missing
%%   in the closing sequence in the includeSect in the file ibm62n08.dtd.
'ibm-not-wf-P62-ibm62n08'(Config) -> run_test(Config, "ibm", "not-wf/P62/ibm62n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n01.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with wrong key word. The string "ignore" is used as
%%   a key word in the beginning sequence in the ignoreSect in the file
%%   ibm63n01.dtd.
'ibm-not-wf-P63-ibm63n01'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n02.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with wrong beginning sequence. An extra "[" occurs
%%   in the beginning sequence in the ignoreSect in the file
%%   ibm63n02.dtd.
'ibm-not-wf-P63-ibm63n02'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n03.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with wrong beginning sequence. A wrong character
%%   "?" occurs in the beginning sequence in the ignoreSect in the file
%%   ibm63n03.dtd.
'ibm-not-wf-P63-ibm63n03'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n04.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with a required field missing. The key word
%%   "IGNORE" is missing in the ignoreSect in the file ibm63n04.dtd.
'ibm-not-wf-P63-ibm63n04'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n05.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with a required field missing. The "[" is missing
%%   after the key word "IGNORE" in the ignoreSect in the file
%%   ibm63n05.dtd.
'ibm-not-wf-P63-ibm63n05'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n06.xml
%% Entities: parameter
%% Description:
%%   Tests includeSect with wrong field ordering. The two external subset
%%   declarations occur before the key word "IGNORE" in the ignoreSect in
%%   the file ibm63n06.dtd.
'ibm-not-wf-P63-ibm63n06'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P63-ibm63n07.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSect with a required field missing. The closing sequence
%%   "]](greater than)" is missing in the ignoreSect in the file
%%   ibm63n07.dtd.
'ibm-not-wf-P63-ibm63n07'(Config) -> run_test(Config, "ibm", "not-wf/P63/ibm63n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P64-ibm64n01.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSectContents with wrong beginning sequence. The "?"
%%   occurs in beginning sequence the ignoreSectContents in the file
%%   ibm64n01.dtd.
'ibm-not-wf-P64-ibm64n01'(Config) -> run_test(Config, "ibm", "not-wf/P64/ibm64n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P64-ibm64n02.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSectContents with a required field missing.The closing
%%   sequence is missing in the ignoreSectContents in the file
%%   ibm64n02.dtd.
'ibm-not-wf-P64-ibm64n02'(Config) -> run_test(Config, "ibm", "not-wf/P64/ibm64n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P64-ibm64n03.xml
%% Entities: parameter
%% Description:
%%   Tests ignoreSectContents with a required field missing.The beginning
%%   sequence is missing in the ignoreSectContents in the file
%%   ibm64n03.dtd.
'ibm-not-wf-P64-ibm64n03'(Config) -> run_test(Config, "ibm", "not-wf/P64/ibm64n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P65-ibm65n01.xml
%% Entities: parameter
%% Description:
%%   Tests Ignore with illegal string included. The string "]](greater
%%   than)" is contained before "this" in the Ignore in the
%%   ignoreSectContents in the file ibm65n01.dtd
'ibm-not-wf-P65-ibm65n01'(Config) -> run_test(Config, "ibm", "not-wf/P65/ibm65n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P65-ibm65n02.xml
%% Entities: parameter
%% Description:
%%   Tests Ignore with illegal string included. The string "(less
%%   than)![" is contained before "this" in the Ignore in the
%%   ignoreSectContents in the file ibm65n02.dtd
'ibm-not-wf-P65-ibm65n02'(Config) -> run_test(Config, "ibm", "not-wf/P65/ibm65n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n01.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#002f" is
%%   used as the referred character in the CharRef in the EntityDecl in
%%   the DTD.
'ibm-not-wf-P66-ibm66n01'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n02.xml
%% Description:
%%   Tests CharRef with the semicolon character missing. The semicolon
%%   character is missing at the end of the CharRef in the attribute
%%   value in the STag of element "root".
'ibm-not-wf-P66-ibm66n02'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n03.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "49" is
%%   used as the referred character in the CharRef in the EntityDecl in
%%   the DTD.
'ibm-not-wf-P66-ibm66n03'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n04.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#5~0" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n04'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n05.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#x002g" is
%%   used as the referred character in the CharRef in the EntityDecl in
%%   the DTD.
'ibm-not-wf-P66-ibm66n05'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n06.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#x006G" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n06'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n07.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#0=2f" is
%%   used as the referred character in the CharRef in the EntityDecl in
%%   the DTD.
'ibm-not-wf-P66-ibm66n07'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n08.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#56.0" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n08'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n09.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#x00/2f"
%%   is used as the referred character in the CharRef in the EntityDecl
%%   in the DTD.
'ibm-not-wf-P66-ibm66n09'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n10.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#51)" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n10'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n11.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#00 2f" is
%%   used as the referred character in the CharRef in the EntityDecl in
%%   the DTD.
'ibm-not-wf-P66-ibm66n11'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n12.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#x0000" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n12'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n13.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#x001f" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n13'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n14.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#xfffe" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n14'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P66-ibm66n15.xml
%% Description:
%%   Tests CharRef with an illegal character referred to. The "#xffff" is
%%   used as the referred character in the attribute value in the
%%   EmptyElemTag of the element "root".
'ibm-not-wf-P66-ibm66n15'(Config) -> run_test(Config, "ibm", "not-wf/P66/ibm66n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n01.xml
%% Description:
%%   Tests EntityRef with a required field missing. The Name is missing
%%   in the EntityRef in the content of the element "root".
'ibm-not-wf-P68-ibm68n01'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n02.xml
%% Description:
%%   Tests EntityRef with a required field missing. The semicolon is
%%   missing in the EntityRef in the attribute value in the element
%%   "root".
'ibm-not-wf-P68-ibm68n02'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n03.xml
%% Description:
%%   Tests EntityRef with an extra white space. A white space occurs
%%   after the ampersand in the EntityRef in the content of the element
%%   "root".
'ibm-not-wf-P68-ibm68n03'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n04.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: Entity Declared. The name
%%   "aAa" in the EntityRef in the AttValue in the STage of the element
%%   "root" does not match the Name of any declared entity in the DTD.
'ibm-not-wf-P68-ibm68n04'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n05.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: Entity Declared. The
%%   entity with the name "aaa" in the EntityRef in the AttValue in the
%%   STag of the element "root" is not declared.
'ibm-not-wf-P68-ibm68n05'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n06.xml
%% Entities: parameter
%% Description:
%%   Tests EntityRef which is against P68 WFC: Entity Declared. The
%%   entity with the name "aaa" in the EntityRef in the AttValue in the
%%   STag of the element "root" is externally declared, but standalone is
%%   "yes".

%% run_test(Config, "ibm", "not-wf/P68/ibm68n06.xml", "not-wf").
'ibm-not-wf-P68-ibm68n06'(_Config) -> {skip, "ISSUE: Violates standalone=yes"}.

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n07.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: Entity Declared. The
%%   entity with the name "aaa" in the EntityRef in the AttValue in the
%%   STag of the element "root" is referred before declared.
'ibm-not-wf-P68-ibm68n07'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n08.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: Parsed Entity. The
%%   EntityRef in the AttValue in the STag of the element "root" contains
%%   the name "aImage" of an unparsed entity.
'ibm-not-wf-P68-ibm68n08'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n09.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: No Recursion. The
%%   recursive entity reference occurs with the entity declarations for
%%   "aaa" and "bbb" in the DTD.
'ibm-not-wf-P68-ibm68n09'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P68-ibm68n10.xml
%% Description:
%%   Tests EntityRef which is against P68 WFC: No Recursion. The indirect
%%   recursive entity reference occurs with the entity declarations for
%%   "aaa", "bbb", "ccc", "ddd", and "eee" in the DTD.
'ibm-not-wf-P68-ibm68n10'(Config) -> run_test(Config, "ibm", "not-wf/P68/ibm68n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n01.xml
%% Description:
%%   Tests PEReference with a required field missing. The Name "paaa" is
%%   missing in the PEReference in the DTD.
'ibm-not-wf-P69-ibm69n01'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n02.xml
%% Description:
%%   Tests PEReference with a required field missing. The semicolon is
%%   missing in the PEReference "%paaa" in the DTD.
'ibm-not-wf-P69-ibm69n02'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n03.xml
%% Description:
%%   Tests PEReference with an extra white space. There is an extra white
%%   space occurs before ";" in the PEReference in the DTD.
'ibm-not-wf-P69-ibm69n03'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n04.xml
%% Description:
%%   Tests PEReference with an extra white space. There is an extra white
%%   space occurs after "%" in the PEReference in the DTD.
'ibm-not-wf-P69-ibm69n04'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n05.xml
%% Description:
%%   Based on E29 substantial source: minutes XML-Syntax 1999-02-24 E38
%%   in XML 1.0 Errata, this WFC does not apply to P69, but the VC Entity
%%   declared still apply. Tests PEReference which is against P69 WFC:
%%   Entity Declared. The PE with the name "paaa" is referred before
%%   declared in the DTD.
'ibm-not-wf-P69-ibm69n05'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n05.xml", "error").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n06.xml
%% Description:
%%   Tests PEReference which is against P69 WFC: No Recursion. The
%%   recursive PE reference occurs with the entity declarations for
%%   "paaa" and "bbb" in the DTD.
'ibm-not-wf-P69-ibm69n06'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P69-ibm69n07.xml
%% Description:
%%   Tests PEReference which is against P69 WFC: No Recursion. The
%%   indirect recursive PE reference occurs with the entity declarations
%%   for "paaa", "bbb", "ccc", "ddd", and "eee" in the DTD.
'ibm-not-wf-P69-ibm69n07'(Config) -> run_test(Config, "ibm", "not-wf/P69/ibm69n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm70n01.xml
%% Description:
%%   Tests
'ibm-not-wf-P71-ibm70n01'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm70n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n01.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The white space is
%%   missing between the beginning sequence and the Name "aaa" in the
%%   EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n01'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n02.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The white space is
%%   missing between the Name "aaa" and the EntityDef "aString" in the
%%   EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n02'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n03.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The EntityDef is
%%   missing in the EntityDecl with the Name "aaa" in the DTD.
'ibm-not-wf-P71-ibm71n03'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n04.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The Name is missing
%%   in the EntityDecl with the EntityDef "aString" in the DTD.
'ibm-not-wf-P71-ibm71n04'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n05.xml
%% Description:
%%   Tests EntityDecl with wrong ordering. The Name "aaa" occurs after
%%   the EntityDef in the EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n05'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n06.xml
%% Description:
%%   Tests EntityDecl with wrong key word. The string "entity" is used as
%%   the key word in the beginning sequence in the EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n06'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n07.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The closing bracket
%%   (greater than) is missing in the EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n07'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P71-ibm71n08.xml
%% Description:
%%   Tests EntityDecl with a required field missing. The exclamation mark
%%   is missing in the beginning sequence in the EntityDecl in the DTD.
'ibm-not-wf-P71-ibm71n08'(Config) -> run_test(Config, "ibm", "not-wf/P71/ibm71n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n01.xml
%% Description:
%%   Tests PEdecl with a required field missing. The white space is
%%   missing between the beginning sequence and the "%" in the PEDecl in
%%   the DTD.
'ibm-not-wf-P72-ibm72n01'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n02.xml
%% Description:
%%   Tests PEdecl with a required field missing. The Name is missing in
%%   the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n02'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n03.xml
%% Description:
%%   Tests PEdecl with a required field missing. The white space is
%%   missing between the Name and the PEDef in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n03'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n04.xml
%% Description:
%%   Tests PEdecl with a required field missing. The PEDef is missing
%%   after the Name "paaa" in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n04'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n05.xml
%% Description:
%%   Tests PEdecl with wrong field ordering. The Name "paaa" occurs after
%%   the PEDef in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n05'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n06.xml
%% Description:
%%   Tests PEdecl with wrong field ordering. The "%" and the Name "paaa"
%%   occurs after the PEDef in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n06'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n07.xml
%% Description:
%%   Tests PEdecl with wrong key word. The string "entity" is used as the
%%   key word in the beginning sequence in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n07'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n08.xml
%% Description:
%%   Tests PEdecl with a required field missing. The closing bracket
%%   (greater than) is missing in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n08'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P72-ibm72n09.xml
%% Description:
%%   Tests PEdecl with wrong closing sequence. The string "!(greater
%%   than)" is used as the closing sequence in the PEDecl in the DTD.
'ibm-not-wf-P72-ibm72n09'(Config) -> run_test(Config, "ibm", "not-wf/P72/ibm72n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P73-ibm73n01.xml
%% Description:
%%   Tests EntityDef with wrong field ordering. The NDataDecl "NDATA
%%   JPGformat" occurs before the ExternalID in the EntityDef in the
%%   EntityDecl.
'ibm-not-wf-P73-ibm73n01'(Config) -> run_test(Config, "ibm", "not-wf/P73/ibm73n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P73-ibm73n03.xml
%% Description:
%%   Tests EntityDef with a required field missing. The ExternalID is
%%   missing before the NDataDecl in the EntityDef in the EntityDecl.
'ibm-not-wf-P73-ibm73n03'(Config) -> run_test(Config, "ibm", "not-wf/P73/ibm73n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P74-ibm74n01.xml
%% Description:
%%   Tests PEDef with extra fields. The NDataDecl occurs after the
%%   ExternalID in the PEDef in the PEDecl in the DTD.
'ibm-not-wf-P74-ibm74n01'(Config) -> run_test(Config, "ibm", "not-wf/P74/ibm74n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n01.xml
%% Description:
%%   Tests ExternalID with wrong key word. The string "system" is used as
%%   the key word in the ExternalID in the EntityDef in the EntityDecl.
'ibm-not-wf-P75-ibm75n01'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n02.xml
%% Description:
%%   Tests ExternalID with wrong key word. The string "public" is used as
%%   the key word in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n02'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n03.xml
%% Description:
%%   Tests ExternalID with wrong key word. The string "Public" is used as
%%   the key word in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n03'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n04.xml
%% Description:
%%   Tests ExternalID with wrong field ordering. The key word "PUBLIC"
%%   occurs after the PublicLiteral and the SystemLiteral in the
%%   ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n04'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n05.xml
%% Description:
%%   Tests ExternalID with a required field missing. The white space
%%   between "SYSTEM" and the Systemliteral is missing in the ExternalID
%%   in the EntityDef in the EntityDecl in the DTD.
'ibm-not-wf-P75-ibm75n05'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n06.xml
%% Description:
%%   Tests ExternalID with a required field missing. The Systemliteral is
%%   missing after "SYSTEM" in the ExternalID in the EntityDef in the
%%   EntityDecl in the DTD.
'ibm-not-wf-P75-ibm75n06'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n07.xml
%% Description:
%%   Tests ExternalID with a required field missing. The white space
%%   between the PublicLiteral and the Systemliteral is missing in the
%%   ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n07'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n08.xml
%% Description:
%%   Tests ExternalID with a required field missing. The key word
%%   "PUBLIC" is missing in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n08'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n09.xml
%% Description:
%%   Tests ExternalID with a required field missing. The white space
%%   between "PUBLIC" and the PublicLiteral is missing in the ExternalID
%%   in the doctypedecl.
'ibm-not-wf-P75-ibm75n09'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n10.xml
%% Description:
%%   Tests ExternalID with a required field missing. The PublicLiteral is
%%   missing in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n10'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n11.xml
%% Description:
%%   Tests ExternalID with a required field missing. The PublicLiteral is
%%   missing in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n11'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n12.xml
%% Description:
%%   Tests ExternalID with a required field missing. The SystemLiteral is
%%   missing in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n12'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P75-ibm75n13.xml
%% Description:
%%   Tests ExternalID with wrong field ordering. The key word "PUBLIC"
%%   occurs after the PublicLiteral in the ExternalID in the doctypedecl.
'ibm-not-wf-P75-ibm75n13'(Config) -> run_test(Config, "ibm", "not-wf/P75/ibm75n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n01.xml
%% Description:
%%   Tests NDataDecl with wrong key word. The string "ndata" is used as
%%   the key word in the NDataDecl in the EntityDef in the GEDecl.
'ibm-not-wf-P76-ibm76n01'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n02.xml
%% Description:
%%   Tests NDataDecl with wrong key word. The string "NData" is used as
%%   the key word in the NDataDecl in the EntityDef in the GEDecl.
'ibm-not-wf-P76-ibm76n02'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n03.xml
%% Description:
%%   Tests NDataDecl with a required field missing. The leading white
%%   space is missing in the NDataDecl in the EntityDef in the GEDecl.
'ibm-not-wf-P76-ibm76n03'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n04.xml
%% Description:
%%   Tests NDataDecl with a required field missing. The key word "NDATA"
%%   is missing in the NDataDecl in the EntityDef in the GEDecl.
'ibm-not-wf-P76-ibm76n04'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n05.xml
%% Description:
%%   Tests NDataDecl with a required field missing. The Name after the
%%   key word "NDATA" is missing in the NDataDecl in the EntityDef in the
%%   GEDecl.
'ibm-not-wf-P76-ibm76n05'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n06.xml
%% Description:
%%   Tests NDataDecl with a required field missing. The white space
%%   between "NDATA" and the Name is missing in the NDataDecl in the
%%   EntityDef in the GEDecl.
'ibm-not-wf-P76-ibm76n06'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P76-ibm76n07.xml
%% Description:
%%   Tests NDataDecl with wrong field ordering. The key word "NDATA"
%%   occurs after the Name in the NDataDecl in the EntityDef in the
%%   GEDecl.
'ibm-not-wf-P76-ibm76n07'(Config) -> run_test(Config, "ibm", "not-wf/P76/ibm76n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P77-ibm77n01.xml
%% Entities: general
%% Description:
%%   Tests TextDecl with wrong field ordering. The VersionInfo occurs
%%   after the EncodingDecl in the TextDecl in the file "ibm77n01.ent".
'ibm-not-wf-P77-ibm77n01'(Config) -> run_test(Config, "ibm", "not-wf/P77/ibm77n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P77-ibm77n02.xml
%% Entities: general
%% Description:
%%   Tests TextDecl with wrong key word. The string "XML" is used in the
%%   beginning sequence in the TextDecl in the file "ibm77n02.ent".
'ibm-not-wf-P77-ibm77n02'(Config) -> run_test(Config, "ibm", "not-wf/P77/ibm77n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P77-ibm77n03.xml
%% Entities: parameter
%% Description:
%%   Tests TextDecl with wrong closing sequence. The character "greater
%%   than" is used as the closing sequence in the TextDecl in the file
%%   "ibm77n03.ent".
'ibm-not-wf-P77-ibm77n03'(Config) -> run_test(Config, "ibm", "not-wf/P77/ibm77n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P77-ibm77n04.xml
%% Entities: parameter
%% Description:
%%   Tests TextDecl with a required field missing. The closing sequence
%%   is missing in the TextDecl in the file "ibm77n04.ent".
'ibm-not-wf-P77-ibm77n04'(Config) -> run_test(Config, "ibm", "not-wf/P77/ibm77n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P78-ibm78n01.xml
%% Entities: general
%% Description:
%%   Tests extParsedEnt with wrong field ordering. The TextDecl occurs
%%   after the content in the file ibm78n01.ent.
'ibm-not-wf-P78-ibm78n01'(Config) -> run_test(Config, "ibm", "not-wf/P78/ibm78n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P78-ibm78n02.xml
%% Entities: general
%% Description:
%%   Tests extParsedEnt with extra field. A blank line occurs before the
%%   TextDecl in the file ibm78n02.ent.
'ibm-not-wf-P78-ibm78n02'(Config) -> run_test(Config, "ibm", "not-wf/P78/ibm78n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P79-ibm79n01.xml
%% Entities: parameter
%% Description:
%%   Tests extPE with wrong field ordering. The TextDecl occurs after the
%%   extSubsetDecl (the white space and the comment) in the file
%%   ibm79n01.ent.
'ibm-not-wf-P79-ibm79n01'(Config) -> run_test(Config, "ibm", "not-wf/P79/ibm79n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P79-ibm79n02.xml
%% Entities: parameter
%% Description:
%%   Tests extPE with extra field. A blank line occurs before the
%%   TextDecl in the file ibm78n02.ent.
'ibm-not-wf-P79-ibm79n02'(Config) -> run_test(Config, "ibm", "not-wf/P79/ibm79n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n01.xml
%% Description:
%%   Tests EncodingDecl with a required field missing. The leading white
%%   space is missing in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P80-ibm80n01'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n02.xml
%% Description:
%%   Tests EncodingDecl with a required field missing. The "=" sign is
%%   missing in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P80-ibm80n02'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n03.xml
%% Description:
%%   Tests EncodingDecl with a required field missing. The double quoted
%%   EncName are missing in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P80-ibm80n03'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n04.xml
%% Description:
%%   Tests EncodingDecl with wrong field ordering. The string "encoding="
%%   occurs after the double quoted EncName in the EncodingDecl in the
%%   XMLDecl.
'ibm-not-wf-P80-ibm80n04'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n05.xml
%% Description:
%%   Tests EncodingDecl with wrong field ordering. The "encoding" occurs
%%   after the double quoted EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P80-ibm80n05'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P80-ibm80n06.xml
%% Description:
%%   Tests EncodingDecl with wrong key word. The string "Encoding" is
%%   used as the key word in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P80-ibm80n06'(Config) -> run_test(Config, "ibm", "not-wf/P80/ibm80n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n01.xml
%% Description:
%%   Tests EncName with an illegal character. The "_" is used as the
%%   first character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n01'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n02.xml
%% Description:
%%   Tests EncName with an illegal character. The "-" is used as the
%%   first character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n02'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n03.xml
%% Description:
%%   Tests EncName with an illegal character. The "." is used as the
%%   first character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n03'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n04.xml
%% Description:
%%   Tests EncName with illegal characters. The "8-" is used as the
%%   initial characters in the EncName in the EncodingDecl in the
%%   XMLDecl.
'ibm-not-wf-P81-ibm81n04'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n05.xml
%% Description:
%%   Tests EncName with an illegal character. The "~" is used as one
%%   character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n05'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n06.xml
%% Description:
%%   Tests EncName with an illegal character. The "#" is used as one
%%   character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n06'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n07.xml
%% Description:
%%   Tests EncName with an illegal character. The ":" is used as one
%%   character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n07'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n08.xml
%% Description:
%%   Tests EncName with an illegal character. The "/" is used as one
%%   character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n08'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P81-ibm81n09.xml
%% Description:
%%   Tests EncName with an illegal character. The ";" is used as one
%%   character in the EncName in the EncodingDecl in the XMLDecl.
'ibm-not-wf-P81-ibm81n09'(Config) -> run_test(Config, "ibm", "not-wf/P81/ibm81n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n01.xml
%% Description:
%%   Tests NotationDecl with a required field missing. The white space
%%   after the beginning sequence of the NotationDecl is missing in the
%%   DTD.
'ibm-not-wf-P82-ibm82n01'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n02.xml
%% Description:
%%   Tests NotationDecl with a required field missing. The Name in the
%%   NotationDecl is missing in the DTD.
'ibm-not-wf-P82-ibm82n02'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n03.xml
%% Description:
%%   Tests NotationDecl with a required field missing. The externalID or
%%   the PublicID is missing in the NotationDecl in the DTD.
'ibm-not-wf-P82-ibm82n03'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n04.xml
%% Description:
%%   Tests NotationDecl with wrong field ordering. The Name occurs after
%%   the "SYSTEM" and the externalID in the NotationDecl in the DTD.
'ibm-not-wf-P82-ibm82n04'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n05.xml
%% Description:
%%   Tests NotationDecl with wrong key word. The string "notation" is
%%   used as a key word in the NotationDecl in the DTD.
'ibm-not-wf-P82-ibm82n05'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n06.xml
%% Description:
%%   Tests NotationDecl with a required field missing. The closing
%%   bracket (the greater than character) is missing in the NotationDecl.
'ibm-not-wf-P82-ibm82n06'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n07.xml
%% Description:
%%   Tests NotationDecl with wrong beginning sequence. The "!" is missing
%%   in the beginning sequence in the NotationDecl in the DTD.
'ibm-not-wf-P82-ibm82n07'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P82-ibm82n08.xml
%% Description:
%%   Tests NotationDecl with wrong closing sequence. The extra "!" occurs
%%   in the closing sequence in the NotationDecl in the DTD.
'ibm-not-wf-P82-ibm82n08'(Config) -> run_test(Config, "ibm", "not-wf/P82/ibm82n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n01.xml
%% Description:
%%   Tests PublicID with wrong key word. The string "public" is used as
%%   the key word in the PublicID in the NotationDecl in the DTD.
'ibm-not-wf-P83-ibm83n01'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n02.xml
%% Description:
%%   Tests PublicID with wrong key word. The string "Public" is used as
%%   the key word in the PublicID in the NotationDecl in the DTD.
'ibm-not-wf-P83-ibm83n02'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n03.xml
%% Description:
%%   Tests PublicID with a required field missing. The key word "PUBLIC"
%%   is missing in the PublicID in the NotationDecl in the DTD.
'ibm-not-wf-P83-ibm83n03'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n04.xml
%% Description:
%%   Tests PublicID with a required field missing. The white space
%%   between the "PUBLIC" and the PubidLiteral is missing in the PublicID
%%   in the NotationDecl in the DTD.
'ibm-not-wf-P83-ibm83n04'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n05.xml
%% Description:
%%   Tests PublicID with a required field missing. The PubidLiteral is
%%   missing in the PublicID in the NotationDecl in the DTD.
'ibm-not-wf-P83-ibm83n05'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P83-ibm83n06.xml
%% Description:
%%   Tests PublicID with wrong field ordering. The key word "PUBLIC"
%%   occurs after the PubidLiteral in the PublicID in the NotationDecl.
'ibm-not-wf-P83-ibm83n06'(Config) -> run_test(Config, "ibm", "not-wf/P83/ibm83n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P85-ibm85n01.xml
%% Description:
%%   Tests BaseChar with an illegal character. The character #x00D7
%%   occurs as the first character of the PITarget in the PI in the DTD.
'ibm-not-wf-P85-ibm85n01'(Config) -> run_test(Config, "ibm", "not-wf/P85/ibm85n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P85-ibm85n02.xml
%% Description:
%%   Tests BaseChar with an illegal character. The character #x00F7
%%   occurs as the first character of the PITarget in the PI in the DTD.
'ibm-not-wf-P85-ibm85n02'(Config) -> run_test(Config, "ibm", "not-wf/P85/ibm85n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P88-ibm88n01.xml
%% Description:
%%   Tests Digit with an illegal character. The character #x0029 occurs
%%   as the second character in the PITarget in the PI in the DTD.
'ibm-not-wf-P88-ibm88n01'(Config) -> run_test(Config, "ibm", "not-wf/P88/ibm88n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P88-ibm88n02.xml
%% Description:
%%   Tests Digit with an illegal character. The character #x003B occurs
%%   as the second character in the PITarget in the PI in the DTD.
'ibm-not-wf-P88-ibm88n02'(Config) -> run_test(Config, "ibm", "not-wf/P88/ibm88n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P89-ibm89n01.xml
%% Description:
%%   Tests Extender with an illegal character. The character #x00B6
%%   occurs as the second character in the PITarget in the PI in the DTD.
'ibm-not-wf-P89-ibm89n01'(Config) -> run_test(Config, "ibm", "not-wf/P89/ibm89n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-not-wf-P89-ibm89n02.xml
%% Description:
%%   Tests Extender with an illegal character. The character #x00B8
%%   occurs as the second character in the PITarget in the PI in the DTD.
'ibm-not-wf-P89-ibm89n02'(Config) -> run_test(Config, "ibm", "not-wf/P89/ibm89n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P01-ibm01v01.xml
%% Output: valid/P01/out/ibm01v01.xml
%% Description:
%%   Tests with a xml document consisting of followed by then
'ibm-valid-P01-ibm01v01'(Config) ->
    run_test(Config, "ibm", "valid/P01/ibm01v01.xml", "valid", "valid/P01/out/ibm01v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P02-ibm02v01.xml
%% Description:
%%   This test case covers legal character ranges plus discrete legal
%%   characters for production 02.
'ibm-valid-P02-ibm02v01'(Config) -> run_test(Config, "ibm", "valid/P02/ibm02v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P03-ibm03v01.xml
%% Description:
%%   Tests all 4 legal white space characters - #x20 #x9 #xD #xA
'ibm-valid-P03-ibm03v01'(Config) -> run_test(Config, "ibm", "valid/P03/ibm03v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P09-ibm09v01.xml
%% Output: valid/P09/out/ibm09v01.xml
%% Description:
%%   Empty EntityValue is legal
'ibm-valid-P09-ibm09v01'(Config) ->
    run_test(Config, "ibm", "valid/P09/ibm09v01.xml", "valid", "valid/P09/out/ibm09v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P09-ibm09v02.xml
%% Output: valid/P09/out/ibm09v02.xml
%% Description:
%%   Tests a normal EnitityValue
'ibm-valid-P09-ibm09v02'(Config) ->
    run_test(Config, "ibm", "valid/P09/ibm09v02.xml", "valid", "valid/P09/out/ibm09v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P09-ibm09v03.xml
%% Entities: parameter
%% Output: valid/P09/out/ibm09v03.xml
%% Description:
%%   Tests EnitityValue referencing a Parameter Entity
'ibm-valid-P09-ibm09v03'(Config) ->
    run_test(Config, "ibm", "valid/P09/ibm09v03.xml", "valid", "valid/P09/out/ibm09v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P09-ibm09v04.xml
%% Output: valid/P09/out/ibm09v04.xml
%% Description:
%%   Tests EnitityValue referencing a General Entity
'ibm-valid-P09-ibm09v04'(Config) ->
    run_test(Config, "ibm", "valid/P09/ibm09v04.xml", "valid", "valid/P09/out/ibm09v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P09-ibm09v05.xml
%% Entities: parameter
%% Output: valid/P09/out/ibm09v05.xml
%% Description:
%%   Tests EnitityValue with combination of GE, PE and text, the GE used
%%   is declared in the student.dtd
'ibm-valid-P09-ibm09v05'(Config) ->
    run_test(Config, "ibm", "valid/P09/ibm09v05.xml", "valid", "valid/P09/out/ibm09v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v01.xml
%% Output: valid/P10/out/ibm10v01.xml
%% Description:
%%   Tests empty AttValue with double quotes as the delimiters
'ibm-valid-P10-ibm10v01'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v01.xml", "valid", "valid/P10/out/ibm10v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v02.xml
%% Output: valid/P10/out/ibm10v02.xml
%% Description:
%%   Tests empty AttValue with single quotes as the delimiters
'ibm-valid-P10-ibm10v02'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v02.xml", "valid", "valid/P10/out/ibm10v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v03.xml
%% Output: valid/P10/out/ibm10v03.xml
%% Description:
%%   Test AttValue with double quotes as the delimiters and single quote
%%   inside
'ibm-valid-P10-ibm10v03'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v03.xml", "valid", "valid/P10/out/ibm10v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v04.xml
%% Output: valid/P10/out/ibm10v04.xml
%% Description:
%%   Test AttValue with single quotes as the delimiters and double quote
%%   inside
'ibm-valid-P10-ibm10v04'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v04.xml", "valid", "valid/P10/out/ibm10v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v05.xml
%% Output: valid/P10/out/ibm10v05.xml
%% Description:
%%   Test AttValue with a GE reference and double quotes as the
%%   delimiters
'ibm-valid-P10-ibm10v05'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v05.xml", "valid", "valid/P10/out/ibm10v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v06.xml
%% Output: valid/P10/out/ibm10v06.xml
%% Description:
%%   Test AttValue with a GE reference and single quotes as the
%%   delimiters
'ibm-valid-P10-ibm10v06'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v06.xml", "valid", "valid/P10/out/ibm10v06.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v07.xml
%% Output: valid/P10/out/ibm10v07.xml
%% Description:
%%   testing AttValue with mixed references and text content in double
%%   quotes
'ibm-valid-P10-ibm10v07'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v07.xml", "valid", "valid/P10/out/ibm10v07.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P10-ibm10v08.xml
%% Output: valid/P10/out/ibm10v08.xml
%% Description:
%%   testing AttValue with mixed references and text content in single
%%   quotes
'ibm-valid-P10-ibm10v08'(Config) ->
    run_test(Config, "ibm", "valid/P10/ibm10v08.xml", "valid", "valid/P10/out/ibm10v08.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P11-ibm11v01.xml
%% Output: valid/P11/out/ibm11v01.xml
%% Description:
%%   Tests empty systemliteral using the double quotes
'ibm-valid-P11-ibm11v01'(Config) ->
    run_test(Config, "ibm", "valid/P11/ibm11v01.xml", "valid", "valid/P11/out/ibm11v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P11-ibm11v02.xml
%% Output: valid/P11/out/ibm11v02.xml
%% Description:
%%   Tests empty systemliteral using the single quotes
'ibm-valid-P11-ibm11v02'(Config) ->
    run_test(Config, "ibm", "valid/P11/ibm11v02.xml", "valid", "valid/P11/out/ibm11v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P11-ibm11v03.xml
%% Entities: parameter
%% Output: valid/P11/out/ibm11v03.xml
%% Description:
%%   Tests regular systemliteral using the single quotes
'ibm-valid-P11-ibm11v03'(Config) ->
    run_test(Config, "ibm", "valid/P11/ibm11v03.xml", "valid", "valid/P11/out/ibm11v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P11-ibm11v04.xml
%% Entities: parameter
%% Output: valid/P11/out/ibm11v04.xml
%% Description:
%%   Tests regular systemliteral using the double quotes
'ibm-valid-P11-ibm11v04'(Config) ->
    run_test(Config, "ibm", "valid/P11/ibm11v04.xml", "valid", "valid/P11/out/ibm11v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P12-ibm12v01.xml
%% Entities: parameter
%% Output: valid/P12/out/ibm12v01.xml
%% Description:
%%   Tests empty systemliteral using the double quotes
'ibm-valid-P12-ibm12v01'(Config) ->
    run_test(Config, "ibm", "valid/P12/ibm12v01.xml", "valid", "valid/P12/out/ibm12v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P12-ibm12v02.xml
%% Entities: parameter
%% Output: valid/P12/out/ibm12v02.xml
%% Description:
%%   Tests empty systemliteral using the single quotes
'ibm-valid-P12-ibm12v02'(Config) ->
    run_test(Config, "ibm", "valid/P12/ibm12v02.xml", "valid", "valid/P12/out/ibm12v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P12-ibm12v03.xml
%% Entities: parameter
%% Output: valid/P12/out/ibm12v03.xml
%% Description:
%%   Tests regular systemliteral using the double quotes
'ibm-valid-P12-ibm12v03'(Config) ->
    run_test(Config, "ibm", "valid/P12/ibm12v03.xml", "valid", "valid/P12/out/ibm12v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P12-ibm12v04.xml
%% Entities: parameter
%% Output: valid/P12/out/ibm12v04.xml
%% Description:
%%   Tests regular systemliteral using the single quotes
'ibm-valid-P12-ibm12v04'(Config) ->
    run_test(Config, "ibm", "valid/P12/ibm12v04.xml", "valid", "valid/P12/out/ibm12v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P13-ibm13v01.xml
%% Entities: parameter
%% Output: valid/P13/out/ibm13v01.xml
%% Description:
%%   Testing PubidChar with all legal PubidChar in a PubidLiteral
'ibm-valid-P13-ibm13v01'(Config) ->
    run_test(Config, "ibm", "valid/P13/ibm13v01.xml", "valid", "valid/P13/out/ibm13v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P14-ibm14v01.xml
%% Output: valid/P14/out/ibm14v01.xml
%% Description:
%%   Testing CharData with empty string
'ibm-valid-P14-ibm14v01'(Config) ->
    run_test(Config, "ibm", "valid/P14/ibm14v01.xml", "valid", "valid/P14/out/ibm14v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P14-ibm14v02.xml
%% Output: valid/P14/out/ibm14v02.xml
%% Description:
%%   Testing CharData with white space character
'ibm-valid-P14-ibm14v02'(Config) ->
    run_test(Config, "ibm", "valid/P14/ibm14v02.xml", "valid", "valid/P14/out/ibm14v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P14-ibm14v03.xml
%% Output: valid/P14/out/ibm14v03.xml
%% Description:
%%   Testing CharData with a general text string
'ibm-valid-P14-ibm14v03'(Config) ->
    run_test(Config, "ibm", "valid/P14/ibm14v03.xml", "valid", "valid/P14/out/ibm14v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P15-ibm15v01.xml
%% Output: valid/P15/out/ibm15v01.xml
%% Description:
%%   Tests empty comment
'ibm-valid-P15-ibm15v01'(Config) ->
    run_test(Config, "ibm", "valid/P15/ibm15v01.xml", "valid", "valid/P15/out/ibm15v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P15-ibm15v02.xml
%% Output: valid/P15/out/ibm15v02.xml
%% Description:
%%   Tests comment with regular text
'ibm-valid-P15-ibm15v02'(Config) ->
    run_test(Config, "ibm", "valid/P15/ibm15v02.xml", "valid", "valid/P15/out/ibm15v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P15-ibm15v03.xml
%% Output: valid/P15/out/ibm15v03.xml
%% Description:
%%   Tests comment with one dash inside
'ibm-valid-P15-ibm15v03'(Config) ->
    run_test(Config, "ibm", "valid/P15/ibm15v03.xml", "valid", "valid/P15/out/ibm15v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P15-ibm15v04.xml
%% Output: valid/P15/out/ibm15v04.xml
%% Description:
%%   Tests comment with more comprehensive content
'ibm-valid-P15-ibm15v04'(Config) ->
    run_test(Config, "ibm", "valid/P15/ibm15v04.xml", "valid", "valid/P15/out/ibm15v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P16-ibm16v01.xml
%% Output: valid/P16/out/ibm16v01.xml
%% Description:
%%   Tests PI definition with only PItarget name and nothing else
'ibm-valid-P16-ibm16v01'(Config) ->
    run_test(Config, "ibm", "valid/P16/ibm16v01.xml", "valid", "valid/P16/out/ibm16v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P16-ibm16v02.xml
%% Output: valid/P16/out/ibm16v02.xml
%% Description:
%%   Tests PI definition with only PItarget name and a white space
'ibm-valid-P16-ibm16v02'(Config) ->
    run_test(Config, "ibm", "valid/P16/ibm16v02.xml", "valid", "valid/P16/out/ibm16v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P16-ibm16v03.xml
%% Output: valid/P16/out/ibm16v03.xml
%% Description:
%%   Tests PI definition with PItarget name and text that contains
%%   question mark and right angle
'ibm-valid-P16-ibm16v03'(Config) ->
    run_test(Config, "ibm", "valid/P16/ibm16v03.xml", "valid", "valid/P16/out/ibm16v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P17-ibm17v01.xml
%% Output: valid/P17/out/ibm17v01.xml
%% Description:
%%   Tests PITarget name
'ibm-valid-P17-ibm17v01'(Config) ->
    run_test(Config, "ibm", "valid/P17/ibm17v01.xml", "valid", "valid/P17/out/ibm17v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P18-ibm18v01.xml
%% Output: valid/P18/out/ibm18v01.xml
%% Description:
%%   Tests CDSect with CDStart CData CDEnd
'ibm-valid-P18-ibm18v01'(Config) ->
    run_test(Config, "ibm", "valid/P18/ibm18v01.xml", "valid", "valid/P18/out/ibm18v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P19-ibm19v01.xml
%% Output: valid/P19/out/ibm19v01.xml
%% Description:
%%   Tests CDStart
'ibm-valid-P19-ibm19v01'(Config) ->
    run_test(Config, "ibm", "valid/P19/ibm19v01.xml", "valid", "valid/P19/out/ibm19v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P20-ibm20v01.xml
%% Output: valid/P20/out/ibm20v01.xml
%% Description:
%%   Tests CDATA with empty string
'ibm-valid-P20-ibm20v01'(Config) ->
    run_test(Config, "ibm", "valid/P20/ibm20v01.xml", "valid", "valid/P20/out/ibm20v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P20-ibm20v02.xml
%% Output: valid/P20/out/ibm20v02.xml
%% Description:
%%   Tests CDATA with regular content
'ibm-valid-P20-ibm20v02'(Config) ->
    run_test(Config, "ibm", "valid/P20/ibm20v02.xml", "valid", "valid/P20/out/ibm20v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P21-ibm21v01.xml
%% Output: valid/P21/out/ibm21v01.xml
%% Description:
%%   Tests CDEnd
'ibm-valid-P21-ibm21v01'(Config) ->
    run_test(Config, "ibm", "valid/P21/ibm21v01.xml", "valid", "valid/P21/out/ibm21v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v01.xml
%% Output: valid/P22/out/ibm22v01.xml
%% Description:
%%   Tests prolog with XMLDecl and doctypedecl
'ibm-valid-P22-ibm22v01'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v01.xml", "valid", "valid/P22/out/ibm22v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v02.xml
%% Output: valid/P22/out/ibm22v02.xml
%% Description:
%%   Tests prolog with doctypedecl
'ibm-valid-P22-ibm22v02'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v02.xml", "valid", "valid/P22/out/ibm22v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v03.xml
%% Output: valid/P22/out/ibm22v03.xml
%% Description:
%%   Tests prolog with Misc doctypedecl
'ibm-valid-P22-ibm22v03'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v03.xml", "valid", "valid/P22/out/ibm22v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v04.xml
%% Output: valid/P22/out/ibm22v04.xml
%% Description:
%%   Tests prolog with doctypedecl Misc
'ibm-valid-P22-ibm22v04'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v04.xml", "valid", "valid/P22/out/ibm22v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v05.xml
%% Output: valid/P22/out/ibm22v05.xml
%% Description:
%%   Tests prolog with XMLDecl Misc doctypedecl
'ibm-valid-P22-ibm22v05'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v05.xml", "valid", "valid/P22/out/ibm22v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v06.xml
%% Output: valid/P22/out/ibm22v06.xml
%% Description:
%%   Tests prolog with XMLDecl doctypedecl Misc
'ibm-valid-P22-ibm22v06'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v06.xml", "valid", "valid/P22/out/ibm22v06.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P22-ibm22v07.xml
%% Output: valid/P22/out/ibm22v07.xml
%% Description:
%%   Tests prolog with XMLDecl Misc doctypedecl Misc
'ibm-valid-P22-ibm22v07'(Config) ->
    run_test(Config, "ibm", "valid/P22/ibm22v07.xml", "valid", "valid/P22/out/ibm22v07.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v01.xml
%% Output: valid/P23/out/ibm23v01.xml
%% Description:
%%   Tests XMLDecl with VersionInfo only
'ibm-valid-P23-ibm23v01'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v01.xml", "valid", "valid/P23/out/ibm23v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v02.xml
%% Output: valid/P23/out/ibm23v02.xml
%% Description:
%%   Tests XMLDecl with VersionInfo EncodingDecl
'ibm-valid-P23-ibm23v02'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v02.xml", "valid", "valid/P23/out/ibm23v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v03.xml
%% Output: valid/P23/out/ibm23v03.xml
%% Description:
%%   Tests XMLDecl with VersionInfo SDDecl
'ibm-valid-P23-ibm23v03'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v03.xml", "valid", "valid/P23/out/ibm23v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v04.xml
%% Output: valid/P23/out/ibm23v04.xml
%% Description:
%%   Tests XMLDecl with VerstionInfo and a trailing whitespace char
'ibm-valid-P23-ibm23v04'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v04.xml", "valid", "valid/P23/out/ibm23v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v05.xml
%% Output: valid/P23/out/ibm23v05.xml
%% Description:
%%   Tests XMLDecl with VersionInfo EncodingDecl SDDecl
'ibm-valid-P23-ibm23v05'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v05.xml", "valid", "valid/P23/out/ibm23v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P23-ibm23v06.xml
%% Output: valid/P23/out/ibm23v06.xml
%% Description:
%%   Tests XMLDecl with VersionInfo EncodingDecl SDDecl and a trailing
%%   whitespace
'ibm-valid-P23-ibm23v06'(Config) ->
    run_test(Config, "ibm", "valid/P23/ibm23v06.xml", "valid", "valid/P23/out/ibm23v06.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P24-ibm24v01.xml
%% Output: valid/P24/out/ibm24v01.xml
%% Description:
%%   Tests VersionInfo with single quote
'ibm-valid-P24-ibm24v01'(Config) ->
    run_test(Config, "ibm", "valid/P24/ibm24v01.xml", "valid", "valid/P24/out/ibm24v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P24-ibm24v02.xml
%% Output: valid/P24/out/ibm24v02.xml
%% Description:
%%   Tests VersionInfo with double quote
'ibm-valid-P24-ibm24v02'(Config) ->
    run_test(Config, "ibm", "valid/P24/ibm24v02.xml", "valid", "valid/P24/out/ibm24v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P25-ibm25v01.xml
%% Output: valid/P25/out/ibm25v01.xml
%% Description:
%%   Tests EQ with =
'ibm-valid-P25-ibm25v01'(Config) ->
    run_test(Config, "ibm", "valid/P25/ibm25v01.xml", "valid", "valid/P25/out/ibm25v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P25-ibm25v02.xml
%% Output: valid/P25/out/ibm25v02.xml
%% Description:
%%   Tests EQ with = and spaces on both sides
'ibm-valid-P25-ibm25v02'(Config) ->
    run_test(Config, "ibm", "valid/P25/ibm25v02.xml", "valid", "valid/P25/out/ibm25v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P25-ibm25v03.xml
%% Output: valid/P25/out/ibm25v03.xml
%% Description:
%%   Tests EQ with = and space in front of it
'ibm-valid-P25-ibm25v03'(Config) ->
    run_test(Config, "ibm", "valid/P25/ibm25v03.xml", "valid", "valid/P25/out/ibm25v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P25-ibm25v04.xml
%% Output: valid/P25/out/ibm25v04.xml
%% Description:
%%   Tests EQ with = and space after it
'ibm-valid-P25-ibm25v04'(Config) ->
    run_test(Config, "ibm", "valid/P25/ibm25v04.xml", "valid", "valid/P25/out/ibm25v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P26-ibm26v01.xml
%% Output: valid/P26/out/ibm26v01.xml
%% Description:
%%   Tests VersionNum 1.0
'ibm-valid-P26-ibm26v01'(Config) ->
    run_test(Config, "ibm", "valid/P26/ibm26v01.xml", "valid", "valid/P26/out/ibm26v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P27-ibm27v01.xml
%% Output: valid/P27/out/ibm27v01.xml
%% Description:
%%   Tests Misc with comment
'ibm-valid-P27-ibm27v01'(Config) ->
    run_test(Config, "ibm", "valid/P27/ibm27v01.xml", "valid", "valid/P27/out/ibm27v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P27-ibm27v02.xml
%% Output: valid/P27/out/ibm27v02.xml
%% Description:
%%   Tests Misc with PI
'ibm-valid-P27-ibm27v02'(Config) ->
    run_test(Config, "ibm", "valid/P27/ibm27v02.xml", "valid", "valid/P27/out/ibm27v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P27-ibm27v03.xml
%% Output: valid/P27/out/ibm27v03.xml
%% Description:
%%   Tests Misc with white spaces
'ibm-valid-P27-ibm27v03'(Config) ->
    run_test(Config, "ibm", "valid/P27/ibm27v03.xml", "valid", "valid/P27/out/ibm27v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P28-ibm28v01.xml
%% Output: valid/P28/out/ibm28v01.xml
%% Description:
%%   Tests doctypedecl with internal DTD only
'ibm-valid-P28-ibm28v01'(Config) ->
    run_test(Config, "ibm", "valid/P28/ibm28v01.xml", "valid", "valid/P28/out/ibm28v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P28-ibm28v02.xml
%% Entities: parameter
%% Output: valid/P28/out/ibm28v02.xml
%% Description:
%%   Tests doctypedecl with external subset and combinations of different
%%   markup declarations and PEReferences
'ibm-valid-P28-ibm28v02'(Config) ->
    run_test(Config, "ibm", "valid/P28/ibm28v02.xml", "valid", "valid/P28/out/ibm28v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P29-ibm29v01.xml
%% Output: valid/P29/out/ibm29v01.xml
%% Description:
%%   Tests markupdecl with combinations of elementdecl,
%%   AttlistDecl,EntityDecl, NotationDecl, PI and comment
'ibm-valid-P29-ibm29v01'(Config) ->
    run_test(Config, "ibm", "valid/P29/ibm29v01.xml", "valid", "valid/P29/out/ibm29v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P29-ibm29v02.xml
%% Entities: parameter
%% Output: valid/P29/out/ibm29v02.xml
%% Description:
%%   Tests WFC: PE in internal subset as a positive test
'ibm-valid-P29-ibm29v02'(Config) ->
    run_test(Config, "ibm", "valid/P29/ibm29v02.xml", "valid", "valid/P29/out/ibm29v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P30-ibm30v01.xml
%% Entities: parameter
%% Output: valid/P30/out/ibm30v01.xml
%% Description:
%%   Tests extSubset with extSubsetDecl only in the dtd file
'ibm-valid-P30-ibm30v01'(Config) ->
    run_test(Config, "ibm", "valid/P30/ibm30v01.xml", "valid", "valid/P30/out/ibm30v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P30-ibm30v02.xml
%% Entities: parameter
%% Output: valid/P30/out/ibm30v02.xml
%% Description:
%%   Tests extSubset with TextDecl and extSubsetDecl in the dtd file
'ibm-valid-P30-ibm30v02'(Config) ->
    run_test(Config, "ibm", "valid/P30/ibm30v02.xml", "valid", "valid/P30/out/ibm30v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P31-ibm31v01.xml
%% Entities: parameter
%% Output: valid/P31/out/ibm31v01.xml
%% Description:
%%   Tests extSubsetDecl with combinations of markupdecls,
%%   conditionalSects, PEReferences and white spaces
'ibm-valid-P31-ibm31v01'(Config) ->
    run_test(Config, "ibm", "valid/P31/ibm31v01.xml", "valid", "valid/P31/out/ibm31v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P32-ibm32v01.xml
%% Entities: parameter
%% Output: valid/P32/out/ibm32v01.xml
%% Description:
%%   Tests VC: Standalone Document Declaration with absent attribute that
%%   has default value and standalone is no
'ibm-valid-P32-ibm32v01'(Config) ->
    run_test(Config, "ibm", "valid/P32/ibm32v01.xml", "valid", "valid/P32/out/ibm32v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P32-ibm32v02.xml
%% Entities: parameter
%% Output: valid/P32/out/ibm32v02.xml
%% Description:
%%   Tests VC: Standalone Document Declaration with external entity
%%   reference and standalone is no
'ibm-valid-P32-ibm32v02'(Config) ->
    run_test(Config, "ibm", "valid/P32/ibm32v02.xml", "valid", "valid/P32/out/ibm32v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P32-ibm32v03.xml
%% Entities: parameter
%% Output: valid/P32/out/ibm32v03.xml
%% Description:
%%   Tests VC: Standalone Document Declaration with attribute values that
%%   need to be normalized and standalone is no
'ibm-valid-P32-ibm32v03'(Config) ->
    run_test(Config, "ibm", "valid/P32/ibm32v03.xml", "valid", "valid/P32/out/ibm32v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P32-ibm32v04.xml
%% Entities: parameter
%% Output: valid/P32/out/ibm32v04.xml
%% Description:
%%   Tests VC: Standalone Document Declaration with whitespace in mixed
%%   content and standalone is no
'ibm-valid-P32-ibm32v04'(Config) ->
    run_test(Config, "ibm", "valid/P32/ibm32v04.xml", "valid", "valid/P32/out/ibm32v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P33-ibm33v01.xml
%% Output: valid/P33/out/ibm33v01.xml
%% Description:
%%   Tests LanguageID with Langcode - Subcode
'ibm-valid-P33-ibm33v01'(Config) ->
    run_test(Config, "ibm", "valid/P33/ibm33v01.xml", "valid", "valid/P33/out/ibm33v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P34-ibm34v01.xml
%% Output: valid/P34/out/ibm34v01.xml
%% Description:
%%   Duplicate Test as ibm33v01.xml
'ibm-valid-P34-ibm34v01'(Config) ->
    run_test(Config, "ibm", "valid/P34/ibm34v01.xml", "valid", "valid/P34/out/ibm34v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P35-ibm35v01.xml
%% Output: valid/P35/out/ibm35v01.xml
%% Description:
%%   Tests ISO639Code
'ibm-valid-P35-ibm35v01'(Config) ->
    run_test(Config, "ibm", "valid/P35/ibm35v01.xml", "valid", "valid/P35/out/ibm35v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P36-ibm36v01.xml
%% Output: valid/P36/out/ibm36v01.xml
%% Description:
%%   Tests IanaCode
'ibm-valid-P36-ibm36v01'(Config) ->
    run_test(Config, "ibm", "valid/P36/ibm36v01.xml", "valid", "valid/P36/out/ibm36v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P37-ibm37v01.xml
%% Output: valid/P37/out/ibm37v01.xml
%% Description:
%%   Tests UserCode
'ibm-valid-P37-ibm37v01'(Config) ->
    run_test(Config, "ibm", "valid/P37/ibm37v01.xml", "valid", "valid/P37/out/ibm37v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P38-ibm38v01.xml
%% Output: valid/P38/out/ibm38v01.xml
%% Description:
%%   Tests SubCode
'ibm-valid-P38-ibm38v01'(Config) ->
    run_test(Config, "ibm", "valid/P38/ibm38v01.xml", "valid", "valid/P38/out/ibm38v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P39-ibm39v01.xml
%% Output: valid/P39/out/ibm39v01.xml
%% Description:
%%   Tests element with EmptyElemTag and STag content Etag, also tests
%%   the VC: Element Valid with elements that have children, Mixed and
%%   ANY contents
'ibm-valid-P39-ibm39v01'(Config) ->
    run_test(Config, "ibm", "valid/P39/ibm39v01.xml", "valid", "valid/P39/out/ibm39v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P40-ibm40v01.xml
%% Output: valid/P40/out/ibm40v01.xml
%% Description:
%%   Tests STag with possible combinations of its fields, also tests WFC:
%%   Unique Att Spec.
'ibm-valid-P40-ibm40v01'(Config) ->
    run_test(Config, "ibm", "valid/P40/ibm40v01.xml", "valid", "valid/P40/out/ibm40v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P41-ibm41v01.xml
%% Output: valid/P41/out/ibm41v01.xml
%% Description:
%%   Tests Attribute with Name Eq AttValue and VC: Attribute Value Type
'ibm-valid-P41-ibm41v01'(Config) ->
    run_test(Config, "ibm", "valid/P41/ibm41v01.xml", "valid", "valid/P41/out/ibm41v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P42-ibm42v01.xml
%% Output: valid/P42/out/ibm42v01.xml
%% Description:
%%   Tests ETag with possible combinations of its fields
'ibm-valid-P42-ibm42v01'(Config) ->
    run_test(Config, "ibm", "valid/P42/ibm42v01.xml", "valid", "valid/P42/out/ibm42v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P43-ibm43v01.xml
%% Output: valid/P43/out/ibm43v01.xml
%% Description:
%%   Tests content with all possible constructs: element, CharData,
%%   Reference, CDSect, Comment
'ibm-valid-P43-ibm43v01'(Config) ->
    run_test(Config, "ibm", "valid/P43/ibm43v01.xml", "valid", "valid/P43/out/ibm43v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P44-ibm44v01.xml
%% Output: valid/P44/out/ibm44v01.xml
%% Description:
%%   Tests EmptyElemTag with possible combinations of its fields
'ibm-valid-P44-ibm44v01'(Config) ->
    run_test(Config, "ibm", "valid/P44/ibm44v01.xml", "valid", "valid/P44/out/ibm44v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P45-ibm45v01.xml
%% Output: valid/P45/out/ibm45v01.xml
%% Description:
%%   Tests both P45 elementDecl and P46 contentspec with possible
%%   combinations of their constructs
'ibm-valid-P45-ibm45v01'(Config) ->
    run_test(Config, "ibm", "valid/P45/ibm45v01.xml", "valid", "valid/P45/out/ibm45v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P47-ibm47v01.xml
%% Output: valid/P47/out/ibm47v01.xml
%% Description:
%%   Tests all possible children,cp,choice,seq patterns in
%%   P47,P48,P49,P50
'ibm-valid-P47-ibm47v01'(Config) ->
    run_test(Config, "ibm", "valid/P47/ibm47v01.xml", "valid", "valid/P47/out/ibm47v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P49-ibm49v01.xml
%% Entities: parameter
%% Output: valid/P49/out/ibm49v01.xml
%% Description:
%%   Tests VC:Proper Group/PE Nesting with PEs of choices that are
%%   properly nested with parenthesized groups in external subsets
'ibm-valid-P49-ibm49v01'(Config) ->
    run_test(Config, "ibm", "valid/P49/ibm49v01.xml", "valid", "valid/P49/out/ibm49v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P50-ibm50v01.xml
%% Entities: parameter
%% Output: valid/P50/out/ibm50v01.xml
%% Description:
%%   Tests VC:Proper Group/PE Nesting with PEs of seq that are properly
%%   nested with parenthesized groups in external subsets
'ibm-valid-P50-ibm50v01'(Config) ->
    run_test(Config, "ibm", "valid/P50/ibm50v01.xml", "valid", "valid/P50/out/ibm50v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P51-ibm51v01.xml
%% Output: valid/P51/out/ibm51v01.xml
%% Description:
%%   Tests Mixed with possible combinations of its fields amd VC: No
%%   Duplicate Types
'ibm-valid-P51-ibm51v01'(Config) ->
    run_test(Config, "ibm", "valid/P51/ibm51v01.xml", "valid", "valid/P51/out/ibm51v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P51-ibm51v02.xml
%% Entities: parameter
%% Output: valid/P51/out/ibm51v02.xml
%% Description:
%%   Tests VC:Proper Group/PE Nesting with PEs of Mixed that are properly
%%   nested with parenthesized groups in external subsets
'ibm-valid-P51-ibm51v02'(Config) ->
    run_test(Config, "ibm", "valid/P51/ibm51v02.xml", "valid", "valid/P51/out/ibm51v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P52-ibm52v01.xml
%% Output: valid/P52/out/ibm52v01.xml
%% Description:
%%   Tests all AttlistDecl and AttDef Patterns in P52 and P53
'ibm-valid-P52-ibm52v01'(Config) ->
    run_test(Config, "ibm", "valid/P52/ibm52v01.xml", "valid", "valid/P52/out/ibm52v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P54-ibm54v01.xml
%% Description:
%%   Tests all AttTypes : StringType, TokenizedTypes, EnumeratedTypes in
%%   P55,P56,P57,P58,P59. Also tests all DefaultDecls in P60.
'ibm-valid-P54-ibm54v01'(Config) -> run_test(Config, "ibm", "valid/P54/ibm54v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P54-ibm54v02.xml
%% Output: valid/P54/out/ibm54v02.xml
%% Description:
%%   Tests all AttTypes : StringType, TokenizedType, EnumeratedTypes in
%%   P55,P56,P57.
'ibm-valid-P54-ibm54v02'(Config) ->
    run_test(Config, "ibm", "valid/P54/ibm54v02.xml", "valid", "valid/P54/out/ibm54v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P54-ibm54v03.xml
%% Output: valid/P54/out/ibm54v03.xml
%% Description:
%%   Tests AttTypes with StringType in P55.
'ibm-valid-P54-ibm54v03'(Config) ->
    run_test(Config, "ibm", "valid/P54/ibm54v03.xml", "valid", "valid/P54/out/ibm54v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P55-ibm55v01.xml
%% Output: valid/P55/out/ibm55v01.xml
%% Description:
%%   Tests StringType for P55. The "CDATA" occurs in the StringType for
%%   the attribute "att" for the element "a".
'ibm-valid-P55-ibm55v01'(Config) ->
    run_test(Config, "ibm", "valid/P55/ibm55v01.xml", "valid", "valid/P55/out/ibm55v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v01.xml
%% Output: valid/P56/out/ibm56v01.xml
%% Description:
%%   Tests TokenizedType for P56. The "ID", "IDREF", "IDREFS", "ENTITY",
%%   "ENTITIES", "NMTOKEN", and "NMTOKENS" occur in the TokenizedType for
%%   the attribute "attr".
'ibm-valid-P56-ibm56v01'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v01.xml", "valid", "valid/P56/out/ibm56v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v02.xml
%% Output: valid/P56/out/ibm56v02.xml
%% Description:
%%   Tests TokenizedType for P56 VC: ID Attribute Default. The value
%%   "AC1999" is assigned to the ID attribute "attr" with "#REQUIRED" in
%%   the DeaultDecl.
'ibm-valid-P56-ibm56v02'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v02.xml", "valid", "valid/P56/out/ibm56v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v03.xml
%% Output: valid/P56/out/ibm56v03.xml
%% Description:
%%   Tests TokenizedType for P56 VC: ID Attribute Default. The value
%%   "AC1999" is assigned to the ID attribute "attr" with "#IMPLIED" in
%%   the DeaultDecl.
'ibm-valid-P56-ibm56v03'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v03.xml", "valid", "valid/P56/out/ibm56v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v04.xml
%% Output: valid/P56/out/ibm56v04.xml
%% Description:
%%   Tests TokenizedType for P56 VC: ID. The ID attribute "UniqueName"
%%   appears only once in the document.
'ibm-valid-P56-ibm56v04'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v04.xml", "valid", "valid/P56/out/ibm56v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v05.xml
%% Output: valid/P56/out/ibm56v05.xml
%% Description:
%%   Tests TokenizedType for P56 VC: One ID per element type. The element
%%   "a" or "b" has only one ID attribute.
'ibm-valid-P56-ibm56v05'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v05.xml", "valid", "valid/P56/out/ibm56v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v06.xml
%% Output: valid/P56/out/ibm56v06.xml
%% Description:
%%   Tests TokenizedType for P56 VC: IDREF. The IDREF value "AC456"
%%   matches the value assigned to an ID attribute "UniqueName".
'ibm-valid-P56-ibm56v06'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v06.xml", "valid", "valid/P56/out/ibm56v06.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v07.xml
%% Output: valid/P56/out/ibm56v07.xml
%% Description:
%%   Tests TokenizedType for P56 VC: IDREF. The IDREFS value "AC456 Q123"
%%   matches the values assigned to the ID attribute "UniqueName" and
%%   "Uname".
'ibm-valid-P56-ibm56v07'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v07.xml", "valid", "valid/P56/out/ibm56v07.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v08.xml
%% Output: valid/P56/out/ibm56v08.xml
%% Description:
%%   Tests TokenizedType for P56 VC: Entity Name. The value "image" of
%%   the ENTITY attribute "sun" matches the name of an unparsed entity
%%   declared.
'ibm-valid-P56-ibm56v08'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v08.xml", "valid", "valid/P56/out/ibm56v08.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v09.xml
%% Output: valid/P56/out/ibm56v09.xml
%% Description:
%%   Tests TokenizedType for P56 VC: Name Token. The value of the NMTOKEN
%%   attribute "thistoken" matches the Nmtoken production.
'ibm-valid-P56-ibm56v09'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v09.xml", "valid", "valid/P56/out/ibm56v09.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P56-ibm56v10.xml
%% Output: valid/P56/out/ibm56v10.xml
%% Description:
%%   Tests TokenizedType for P56 VC: Name Token. The value of the
%%   NMTOKENS attribute "thistoken" matches the Nmtoken production.
'ibm-valid-P56-ibm56v10'(Config) ->
    run_test(Config, "ibm", "valid/P56/ibm56v10.xml", "valid", "valid/P56/out/ibm56v10.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P57-ibm57v01.xml
%% Output: valid/P57/out/ibm57v01.xml
%% Description:
%%   Tests EnumeratedType in the AttType. The attribute "att" has a type
%%   (a|b) with the element "a". the
'ibm-valid-P57-ibm57v01'(Config) ->
    run_test(Config, "ibm", "valid/P57/ibm57v01.xml", "valid", "valid/P57/out/ibm57v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P58-ibm58v01.xml
%% Output: valid/P58/out/ibm58v01.xml
%% Description:
%%   Tests NotationType for P58. It shows different patterns fro the
%%   NOTATION attribute "attr".
'ibm-valid-P58-ibm58v01'(Config) ->
    run_test(Config, "ibm", "valid/P58/ibm58v01.xml", "valid", "valid/P58/out/ibm58v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P58-ibm58v02.xml
%% Output: valid/P58/out/ibm58v02.xml
%% Description:
%%   Tests NotationType for P58: Notation Attributes. The value "base64"
%%   of the NOTATION attribute "attr" matches one of the notation names
%%   declared.
'ibm-valid-P58-ibm58v02'(Config) ->
    run_test(Config, "ibm", "valid/P58/ibm58v02.xml", "valid", "valid/P58/out/ibm58v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P59-ibm59v01.xml
%% Output: valid/P59/out/ibm59v01.xml
%% Description:
%%   Tests Enumeration in the EnumeratedType for P59. It shows different
%%   patterns for the Enumeration attribute "attr".
'ibm-valid-P59-ibm59v01'(Config) ->
    run_test(Config, "ibm", "valid/P59/ibm59v01.xml", "valid", "valid/P59/out/ibm59v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P59-ibm59v02.xml
%% Output: valid/P59/out/ibm59v02.xml
%% Description:
%%   Tests Enumeration for P59 VC: Enumeration. The value "one" of the
%%   Enumeration attribute "attr" matches one of the element names
%%   declared.
'ibm-valid-P59-ibm59v02'(Config) ->
    run_test(Config, "ibm", "valid/P59/ibm59v02.xml", "valid", "valid/P59/out/ibm59v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P60-ibm60v01.xml
%% Output: valid/P60/out/ibm60v01.xml
%% Description:
%%   Tests DefaultDecl for P60. It shows different options "#REQUIRED",
%%   "#FIXED", "#IMPLIED", and default for the attribute "chapter".
'ibm-valid-P60-ibm60v01'(Config) ->
    run_test(Config, "ibm", "valid/P60/ibm60v01.xml", "valid", "valid/P60/out/ibm60v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P60-ibm60v02.xml
%% Output: valid/P60/out/ibm60v02.xml
%% Description:
%%   Tests DefaultDecl for P60 VC: Required Attribute. In the element
%%   "one" and "two" the value of the #REQUIRED attribute "chapter" is
%%   given.
'ibm-valid-P60-ibm60v02'(Config) ->
    run_test(Config, "ibm", "valid/P60/ibm60v02.xml", "valid", "valid/P60/out/ibm60v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P60-ibm60v03.xml
%% Output: valid/P60/out/ibm60v03.xml
%% Description:
%%   Tests DefaultDecl for P60 VC: Fixed Attribute Default. The value of
%%   the #FIXED attribute "chapter" is exactly the same as the default
%%   value.
'ibm-valid-P60-ibm60v03'(Config) ->
    run_test(Config, "ibm", "valid/P60/ibm60v03.xml", "valid", "valid/P60/out/ibm60v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P60-ibm60v04.xml
%% Output: valid/P60/out/ibm60v04.xml
%% Description:
%%   Tests DefaultDecl for P60 VC: Attribute Default Legal. The default
%%   value specified for the attribute "attr" meets the lexical
%%   constraints of the declared attribute type.
'ibm-valid-P60-ibm60v04'(Config) ->
    run_test(Config, "ibm", "valid/P60/ibm60v04.xml", "valid", "valid/P60/out/ibm60v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P61-ibm61v01.xml
%% Entities: parameter
%% Output: valid/P61/out/ibm61v01.xml
%% Description:
%%   Tests conditionalSect for P61. It takes the option "invludeSect" in
%%   the file ibm61v01.dtd.
'ibm-valid-P61-ibm61v01'(Config) ->
    run_test(Config, "ibm", "valid/P61/ibm61v01.xml", "valid", "valid/P61/out/ibm61v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P61-ibm61v02.xml
%% Entities: parameter
%% Output: valid/P61/out/ibm61v02.xml
%% Description:
%%   Tests conditionalSect for P61. It takes the option "ignoreSect" in
%%   the file ibm61v02.dtd.
'ibm-valid-P61-ibm61v02'(Config) ->
    run_test(Config, "ibm", "valid/P61/ibm61v02.xml", "valid", "valid/P61/out/ibm61v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P62-ibm62v01.xml
%% Entities: parameter
%% Output: valid/P62/out/ibm62v01.xml
%% Description:
%%   Tests includeSect for P62. The white space is not included before
%%   the key word "INCLUDE" in the beginning sequence.
'ibm-valid-P62-ibm62v01'(Config) ->
    run_test(Config, "ibm", "valid/P62/ibm62v01.xml", "valid", "valid/P62/out/ibm62v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P62-ibm62v02.xml
%% Entities: parameter
%% Output: valid/P62/out/ibm62v02.xml
%% Description:
%%   Tests includeSect for P62. The white space is not included after the
%%   key word "INCLUDE" in the beginning sequence.
'ibm-valid-P62-ibm62v02'(Config) ->
    run_test(Config, "ibm", "valid/P62/ibm62v02.xml", "valid", "valid/P62/out/ibm62v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P62-ibm62v03.xml
%% Entities: parameter
%% Output: valid/P62/out/ibm62v03.xml
%% Description:
%%   Tests includeSect for P62. The white space is included after the key
%%   word "INCLUDE" in the beginning sequence.
'ibm-valid-P62-ibm62v03'(Config) ->
    run_test(Config, "ibm", "valid/P62/ibm62v03.xml", "valid", "valid/P62/out/ibm62v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P62-ibm62v04.xml
%% Entities: parameter
%% Output: valid/P62/out/ibm62v04.xml
%% Description:
%%   Tests includeSect for P62. The white space is included before the
%%   key word "INCLUDE" in the beginning sequence.
'ibm-valid-P62-ibm62v04'(Config) ->
    run_test(Config, "ibm", "valid/P62/ibm62v04.xml", "valid", "valid/P62/out/ibm62v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P62-ibm62v05.xml
%% Entities: parameter
%% Output: valid/P62/out/ibm62v05.xml
%% Description:
%%   Tests includeSect for P62. The extSubsetDecl is not included.
'ibm-valid-P62-ibm62v05'(Config) ->
    run_test(Config, "ibm", "valid/P62/ibm62v05.xml", "valid", "valid/P62/out/ibm62v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P63-ibm63v01.xml
%% Entities: parameter
%% Output: valid/P63/out/ibm63v01.xml
%% Description:
%%   Tests ignoreSect for P63. The white space is not included before the
%%   key word "IGNORE" in the beginning sequence.
'ibm-valid-P63-ibm63v01'(Config) ->
    run_test(Config, "ibm", "valid/P63/ibm63v01.xml", "valid", "valid/P63/out/ibm63v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P63-ibm63v02.xml
%% Entities: parameter
%% Output: valid/P63/out/ibm63v02.xml
%% Description:
%%   Tests ignoreSect for P63. The white space is not included after the
%%   key word "IGNORE" in the beginning sequence.
'ibm-valid-P63-ibm63v02'(Config) ->
    run_test(Config, "ibm", "valid/P63/ibm63v02.xml", "valid", "valid/P63/out/ibm63v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P63-ibm63v03.xml
%% Entities: parameter
%% Output: valid/P63/out/ibm63v03.xml
%% Description:
%%   Tests ignoreSect for P63. The white space is included after the key
%%   word "IGNORE" in the beginning sequence.
'ibm-valid-P63-ibm63v03'(Config) ->
    run_test(Config, "ibm", "valid/P63/ibm63v03.xml", "valid", "valid/P63/out/ibm63v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P63-ibm63v04.xml
%% Entities: parameter
%% Output: valid/P63/out/ibm63v04.xml
%% Description:
%%   Tests ignoreSect for P63. The ignireSectContents is included.
'ibm-valid-P63-ibm63v04'(Config) ->
    run_test(Config, "ibm", "valid/P63/ibm63v04.xml", "valid", "valid/P63/out/ibm63v04.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P63-ibm63v05.xml
%% Entities: parameter
%% Output: valid/P63/out/ibm63v05.xml
%% Description:
%%   Tests ignoreSect for P63. The white space is included before and
%%   after the key word "IGNORE" in the beginning sequence.
'ibm-valid-P63-ibm63v05'(Config) ->
    run_test(Config, "ibm", "valid/P63/ibm63v05.xml", "valid", "valid/P63/out/ibm63v05.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P64-ibm64v01.xml
%% Entities: parameter
%% Output: valid/P64/out/ibm64v01.xml
%% Description:
%%   Tests ignoreSectContents for P64. One "ignore" field is included.
'ibm-valid-P64-ibm64v01'(Config) ->
    run_test(Config, "ibm", "valid/P64/ibm64v01.xml", "valid", "valid/P64/out/ibm64v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P64-ibm64v02.xml
%% Entities: parameter
%% Output: valid/P64/out/ibm64v02.xml
%% Description:
%%   Tests ignoreSectContents for P64. Two "ignore" and one
%%   "ignoreSectContents" fields are included.
'ibm-valid-P64-ibm64v02'(Config) ->
    run_test(Config, "ibm", "valid/P64/ibm64v02.xml", "valid", "valid/P64/out/ibm64v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P64-ibm64v03.xml
%% Entities: parameter
%% Output: valid/P64/out/ibm64v03.xml
%% Description:
%%   Tests ignoreSectContents for P64. Four "ignore" and three
%%   "ignoreSectContents" fields are included.
'ibm-valid-P64-ibm64v03'(Config) ->
    run_test(Config, "ibm", "valid/P64/ibm64v03.xml", "valid", "valid/P64/out/ibm64v03.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P65-ibm65v01.xml
%% Entities: parameter
%% Output: valid/P65/out/ibm65v01.xml
%% Description:
%%   Tests Ignore for P65. An empty string occurs in the Ignore filed.
'ibm-valid-P65-ibm65v01'(Config) ->
    run_test(Config, "ibm", "valid/P65/ibm65v01.xml", "valid", "valid/P65/out/ibm65v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P65-ibm65v02.xml
%% Entities: parameter
%% Output: valid/P65/out/ibm65v02.xml
%% Description:
%%   Tests Ignore for P65. An string not including the brackets occurs in
%%   each of the Ignore filed.
'ibm-valid-P65-ibm65v02'(Config) ->
    run_test(Config, "ibm", "valid/P65/ibm65v02.xml", "valid", "valid/P65/out/ibm65v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P66-ibm66v01.xml
%% Output: valid/P66/out/ibm66v01.xml
%% Description:
%%   Tests all legal CharRef's.
'ibm-valid-P66-ibm66v01'(Config) ->
    run_test(Config, "ibm", "valid/P66/ibm66v01.xml", "valid", "valid/P66/out/ibm66v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P67-ibm67v01.xml
%% Output: valid/P67/out/ibm67v01.xml
%% Description:
%%   Tests Reference could be EntityRef or CharRef.
'ibm-valid-P67-ibm67v01'(Config) ->
    run_test(Config, "ibm", "valid/P67/ibm67v01.xml", "valid", "valid/P67/out/ibm67v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P68-ibm68v01.xml
%% Entities: parameter
%% Output: valid/P68/out/ibm68v01.xml
%% Description:
%%   Tests P68 VC:Entity Declared with Entities in External Subset ,
%%   standalone is no
'ibm-valid-P68-ibm68v01'(Config) ->
    run_test(Config, "ibm", "valid/P68/ibm68v01.xml", "valid", "valid/P68/out/ibm68v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P68-ibm68v02.xml
%% Entities: both
%% Output: valid/P68/out/ibm68v02.xml
%% Description:
%%   Tests P68 VC:Entity Declared with Entities in External Parameter
%%   Entities , standalone is no
'ibm-valid-P68-ibm68v02'(Config) ->
    run_test(Config, "ibm", "valid/P68/ibm68v02.xml", "valid", "valid/P68/out/ibm68v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P69-ibm69v01.xml
%% Entities: parameter
%% Output: valid/P69/out/ibm69v01.xml
%% Description:
%%   Tests P68 VC:Entity Declared with Parameter Entities in External
%%   Subset , standalone is no
'ibm-valid-P69-ibm69v01'(Config) ->
    run_test(Config, "ibm", "valid/P69/ibm69v01.xml", "valid", "valid/P69/out/ibm69v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P69-ibm69v02.xml
%% Entities: both
%% Output: valid/P69/out/ibm69v02.xml
%% Description:
%%   Tests P68 VC:Entity Declared with Parameter Entities in External
%%   Parameter Entities, standalone is no
'ibm-valid-P69-ibm69v02'(Config) ->
    run_test(Config, "ibm", "valid/P69/ibm69v02.xml", "valid", "valid/P69/out/ibm69v02.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P70-ibm70v01.xml
%% Entities: parameter
%% Output: valid/P70/out/ibm70v01.xml
%% Description:
%%   Tests all legal GEDecls and PEDecls constructs derived from P70-76
'ibm-valid-P70-ibm70v01'(Config) ->
    run_test(Config, "ibm", "valid/P70/ibm70v01.xml", "valid", "valid/P70/out/ibm70v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P78-ibm78v01.xml
%% Entities: general
%% Output: valid/P78/out/ibm78v01.xml
%% Description:
%%   Tests ExtParsedEnt, also TextDecl in P77 and EncodingDecl in P80
'ibm-valid-P78-ibm78v01'(Config) ->
    run_test(Config, "ibm", "valid/P78/ibm78v01.xml", "valid", "valid/P78/out/ibm78v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P79-ibm79v01.xml
%% Entities: parameter
%% Description:
%%   Tests extPE
'ibm-valid-P79-ibm79v01'(Config) -> run_test(Config, "ibm", "valid/P79/ibm79v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P82-ibm82v01.xml
%% Output: valid/P82/out/ibm82v01.xml
%% Description:
%%   Tests NotationDecl in P82 and PublicID in P83
'ibm-valid-P82-ibm82v01'(Config) ->
    run_test(Config, "ibm", "valid/P82/ibm82v01.xml", "valid", "valid/P82/out/ibm82v01.xml").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85v01.xml
%% Description:
%%   This test case covers 149 legal character ranges plus 51 single
%%   legal characters for BaseChar in P85 using a PI target Name
'ibm-valid-P85-ibm85v01'(Config) -> run_test(Config, "ibm", "valid/P85/ibm85v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P86-ibm86v01.xml
%% Description:
%%   This test case covers 2 legal character ranges plus 1 single legal
%%   characters for IdeoGraphic in P86 using a PI target Name
'ibm-valid-P86-ibm86v01'(Config) -> run_test(Config, "ibm", "valid/P86/ibm86v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87v01.xml
%% Description:
%%   This test case covers 65 legal character ranges plus 30 single legal
%%   characters for CombiningChar in P87 using a PI target Name
'ibm-valid-P87-ibm87v01'(Config) -> run_test(Config, "ibm", "valid/P87/ibm87v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88v01.xml
%% Description:
%%   This test case covers 15 legal character ranges for Digit in P88
%%   using a PI target Name
'ibm-valid-P88-ibm88v01'(Config) -> run_test(Config, "ibm", "valid/P88/ibm88v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P89-ibm89v01.xml
%% Description:
%%   This test case covers 3 legal character ranges plus 8 single legal
%%   characters for Extender in P89 using a PI target Name
'ibm-valid-P89-ibm89v01'(Config) -> run_test(Config, "ibm", "valid/P89/ibm89v01.xml", "valid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile: IBM XML 1.1 Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P46-ibm46i01.xml
%% Description:
%%   An element with Element-Only content contains the character #x85
%%   (NEL not a whitespace character as defined by S).
'ibm-1-1-valid-P46-ibm46i01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "invalid/P46/ibm46i01.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P46-ibm46i02.xml
%% Description:
%%   An element with Element-Only content contains the character #x2028
%%   (LESP not a whitespace character as defined by S).
'ibm-1-1-valid-P46-ibm46i02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "invalid/P46/ibm46i02.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n01.xml
%% Description:
%%   This test contains embeded control character 0x1.
'ibm-1-1-not-wf-P02-ibm02n01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n02.xml
%% Description:
%%   This test contains embeded control character 0x2.
'ibm-1-1-not-wf-P02-ibm02n02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n03.xml
%% Description:
%%   This test contains embeded control character 0x3.
'ibm-1-1-not-wf-P02-ibm02n03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n04.xml
%% Description:
%%   This test contains embeded control character 0x4.
'ibm-1-1-not-wf-P02-ibm02n04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n05.xml
%% Description:
%%   This test contains embeded control character 0x5.
'ibm-1-1-not-wf-P02-ibm02n05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n06.xml
%% Description:
%%   This test contains embeded control character 0x6.
'ibm-1-1-not-wf-P02-ibm02n06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n07.xml
%% Description:
%%   This test contains embeded control character 0x7.
'ibm-1-1-not-wf-P02-ibm02n07'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n08.xml
%% Description:
%%   This test contains embeded control character 0x8.
'ibm-1-1-not-wf-P02-ibm02n08'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n09.xml
%% Description:
%%   This test contains embeded control character 0x0.
'ibm-1-1-not-wf-P02-ibm02n09'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n10.xml
%% Description:
%%   This test contains embeded control character 0x100.
'ibm-1-1-not-wf-P02-ibm02n10'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n11.xml
%% Description:
%%   This test contains embeded control character 0x0B.
'ibm-1-1-not-wf-P02-ibm02n11'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n12.xml
%% Description:
%%   This test contains embeded control character 0x0C.
'ibm-1-1-not-wf-P02-ibm02n12'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n14.xml
%% Description:
%%   This test contains embeded control character 0x0E.
'ibm-1-1-not-wf-P02-ibm02n14'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n15.xml
%% Description:
%%   This test contains embeded control character 0x0F.
'ibm-1-1-not-wf-P02-ibm02n15'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n16.xml
%% Description:
%%   This test contains embeded control character 0x10.
'ibm-1-1-not-wf-P02-ibm02n16'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n17.xml
%% Description:
%%   This test contains embeded control character 0x11.
'ibm-1-1-not-wf-P02-ibm02n17'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n18.xml
%% Description:
%%   This test contains embeded control character 0x12.
'ibm-1-1-not-wf-P02-ibm02n18'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n19.xml
%% Description:
%%   This test contains embeded control character 0x13.
'ibm-1-1-not-wf-P02-ibm02n19'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n20.xml
%% Description:
%%   This test contains embeded control character 0x14.
'ibm-1-1-not-wf-P02-ibm02n20'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n21.xml
%% Description:
%%   This test contains embeded control character 0x15.
'ibm-1-1-not-wf-P02-ibm02n21'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n22.xml
%% Description:
%%   This test contains embeded control character 0x16.
'ibm-1-1-not-wf-P02-ibm02n22'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n23.xml
%% Description:
%%   This test contains embeded control character 0x17.
'ibm-1-1-not-wf-P02-ibm02n23'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n24.xml
%% Description:
%%   This test contains embeded control character 0x18.
'ibm-1-1-not-wf-P02-ibm02n24'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n25.xml
%% Description:
%%   This test contains embeded control character 0x19.
'ibm-1-1-not-wf-P02-ibm02n25'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n26.xml
%% Description:
%%   This test contains embeded control character 0x1A.
'ibm-1-1-not-wf-P02-ibm02n26'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n27.xml
%% Description:
%%   This test contains embeded control character 0x1B.
'ibm-1-1-not-wf-P02-ibm02n27'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n28.xml
%% Description:
%%   This test contains embeded control character 0x1C.
'ibm-1-1-not-wf-P02-ibm02n28'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n29.xml
%% Description:
%%   This test contains embeded control character 0x1D.
'ibm-1-1-not-wf-P02-ibm02n29'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n29.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n30.xml
%% Description:
%%   This test contains embeded control character 0x1E.
'ibm-1-1-not-wf-P02-ibm02n30'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n30.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n31.xml
%% Description:
%%   This test contains embeded control character 0x1F.
'ibm-1-1-not-wf-P02-ibm02n31'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n31.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n32.xml
%% Description:
%%   This test contains embeded control character 0x7F.
'ibm-1-1-not-wf-P02-ibm02n32'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n32.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n33.xml
%% Description:
%%   This test contains embeded control character 0x80.
'ibm-1-1-not-wf-P02-ibm02n33'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n33.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n34.xml
%% Description:
%%   This test contains embeded control character 0x81.
'ibm-1-1-not-wf-P02-ibm02n34'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n34.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n35.xml
%% Description:
%%   This test contains embeded control character 0x82.
'ibm-1-1-not-wf-P02-ibm02n35'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n35.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n36.xml
%% Description:
%%   This test contains embeded control character 0x83.
'ibm-1-1-not-wf-P02-ibm02n36'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n36.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n37.xml
%% Description:
%%   This test contains embeded control character 0x84.
'ibm-1-1-not-wf-P02-ibm02n37'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n37.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n38.xml
%% Description:
%%   This test contains embeded control characters x82, x83 and x84.
'ibm-1-1-not-wf-P02-ibm02n38'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n38.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n39.xml
%% Description:
%%   This test contains embeded control character 0x86.
'ibm-1-1-not-wf-P02-ibm02n39'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n39.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n40.xml
%% Description:
%%   This test contains embeded control character 0x87.
'ibm-1-1-not-wf-P02-ibm02n40'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n40.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n41.xml
%% Description:
%%   This test contains embeded control character 0x88.
'ibm-1-1-not-wf-P02-ibm02n41'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n41.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n42.xml
%% Description:
%%   This test contains embeded control character 0x89.
'ibm-1-1-not-wf-P02-ibm02n42'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n42.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n43.xml
%% Description:
%%   This test contains embeded control character 0x8A.
'ibm-1-1-not-wf-P02-ibm02n43'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n43.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n44.xml
%% Description:
%%   This test contains embeded control character 0x8B.
'ibm-1-1-not-wf-P02-ibm02n44'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n44.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n45.xml
%% Description:
%%   This test contains embeded control character 0x8C.
'ibm-1-1-not-wf-P02-ibm02n45'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n45.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n46.xml
%% Description:
%%   This test contains embeded control character 0x8D.
'ibm-1-1-not-wf-P02-ibm02n46'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n46.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n47.xml
%% Description:
%%   This test contains embeded control character 0x8E.
'ibm-1-1-not-wf-P02-ibm02n47'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n47.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n48.xml
%% Description:
%%   This test contains embeded control character 0x8F.
'ibm-1-1-not-wf-P02-ibm02n48'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n48.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n49.xml
%% Description:
%%   This test contains embeded control character 0x90.
'ibm-1-1-not-wf-P02-ibm02n49'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n49.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n50.xml
%% Description:
%%   This test contains embeded control character 0x91.
'ibm-1-1-not-wf-P02-ibm02n50'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n50.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n51.xml
%% Description:
%%   This test contains embeded control character 0x92.
'ibm-1-1-not-wf-P02-ibm02n51'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n51.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n52.xml
%% Description:
%%   This test contains embeded control character 0x93.
'ibm-1-1-not-wf-P02-ibm02n52'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n52.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n53.xml
%% Description:
%%   This test contains embeded control character 0x94.
'ibm-1-1-not-wf-P02-ibm02n53'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n53.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n54.xml
%% Description:
%%   This test contains embeded control character 0x95.
'ibm-1-1-not-wf-P02-ibm02n54'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n54.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n55.xml
%% Description:
%%   This test contains embeded control character 0x96.
'ibm-1-1-not-wf-P02-ibm02n55'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n55.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n56.xml
%% Description:
%%   This test contains embeded control character 0x97.
'ibm-1-1-not-wf-P02-ibm02n56'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n56.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n57.xml
%% Description:
%%   This test contains embeded control character 0x98.
'ibm-1-1-not-wf-P02-ibm02n57'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n57.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n58.xml
%% Description:
%%   This test contains embeded control character 0x99.
'ibm-1-1-not-wf-P02-ibm02n58'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n58.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n59.xml
%% Description:
%%   This test contains embeded control character 0x9A.
'ibm-1-1-not-wf-P02-ibm02n59'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n59.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n60.xml
%% Description:
%%   This test contains embeded control character 0x9B.
'ibm-1-1-not-wf-P02-ibm02n60'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n60.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n61.xml
%% Description:
%%   This test contains embeded control character 0x9C.
'ibm-1-1-not-wf-P02-ibm02n61'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n61.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n62.xml
%% Description:
%%   This test contains embeded control character 0x9D.
'ibm-1-1-not-wf-P02-ibm02n62'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n62.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n63.xml
%% Description:
%%   This test contains embeded control character 0x9E.
'ibm-1-1-not-wf-P02-ibm02n63'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n63.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n64.xml
%% Entities: general
%% Description:
%%   This test contains embeded control characters present in an external
%%   entity.
'ibm-1-1-not-wf-P02-ibm02n64'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n64.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n65.xml
%% Entities: general
%% Description:
%%   This test contains embeded control characters present in an external
%%   entity.
'ibm-1-1-not-wf-P02-ibm02n65'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n65.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n66.xml
%% Entities: general
%% Description:
%%   This test contains embeded control characters present in an external
%%   entity.
'ibm-1-1-not-wf-P02-ibm02n66'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n66.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n67.xml
%% Description:
%%   This test contains embeded character 0xD800. (Invalid UTF8 sequence)
'ibm-1-1-not-wf-P02-ibm02n67'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n67.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n68.xml
%% Description:
%%   This test contains embeded character 0xFFFE.
'ibm-1-1-not-wf-P02-ibm02n68'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n68.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n69.xml
%% Description:
%%   This test contains embeded character 0xFFFF.
'ibm-1-1-not-wf-P02-ibm02n69'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n69.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n70.xml
%% Description:
%%   This test contains a reference to character 0xFFFE.
'ibm-1-1-not-wf-P02-ibm02n70'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n70.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P02-ibm02n71.xml
%% Description:
%%   This test contains a reference to character 0xFFFF.
'ibm-1-1-not-wf-P02-ibm02n71'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P02/ibm02n71.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n01.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #x300
'ibm-1-1-not-wf-P04-ibm04n01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n02.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x333
'ibm-1-1-not-wf-P04-ibm04n02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n03.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x369
'ibm-1-1-not-wf-P04-ibm04n03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n04.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x37E
'ibm-1-1-not-wf-P04-ibm04n04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n05.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2000
'ibm-1-1-not-wf-P04-ibm04n05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n06.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2001
'ibm-1-1-not-wf-P04-ibm04n06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n07.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2002
'ibm-1-1-not-wf-P04-ibm04n07'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n08.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2005
'ibm-1-1-not-wf-P04-ibm04n08'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n09.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200B
'ibm-1-1-not-wf-P04-ibm04n09'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n10.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200E
'ibm-1-1-not-wf-P04-ibm04n10'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n11.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200F
'ibm-1-1-not-wf-P04-ibm04n11'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n12.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2069
'ibm-1-1-not-wf-P04-ibm04n12'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n13.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2190
'ibm-1-1-not-wf-P04-ibm04n13'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n14.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x23FF
'ibm-1-1-not-wf-P04-ibm04n14'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n15.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x280F
'ibm-1-1-not-wf-P04-ibm04n15'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n16.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2A00
'ibm-1-1-not-wf-P04-ibm04n16'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n17.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2EDC
'ibm-1-1-not-wf-P04-ibm04n17'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n18.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2B00
'ibm-1-1-not-wf-P04-ibm04n18'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n19.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2BFF
'ibm-1-1-not-wf-P04-ibm04n19'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n20.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x3000
'ibm-1-1-not-wf-P04-ibm04n20'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n21.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xD800
'ibm-1-1-not-wf-P04-ibm04n21'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n22.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xD801
'ibm-1-1-not-wf-P04-ibm04n22'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n23.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xDAFF
'ibm-1-1-not-wf-P04-ibm04n23'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n24.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xDFFF
'ibm-1-1-not-wf-P04-ibm04n24'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n25.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xEFFF
'ibm-1-1-not-wf-P04-ibm04n25'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n26.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xF1FF
'ibm-1-1-not-wf-P04-ibm04n26'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n27.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xF8FF
'ibm-1-1-not-wf-P04-ibm04n27'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04-ibm04n28.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xFFFFF
'ibm-1-1-not-wf-P04-ibm04n28'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04/ibm04n28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an01.xml
%% Description:
%%   Tests an element with an illegal NameChar: #xB8
'ibm-1-1-not-wf-P04a-ibm04an01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an02.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xA1
'ibm-1-1-not-wf-P04a-ibm04an02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an03.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xAF
'ibm-1-1-not-wf-P04a-ibm04an03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an04.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x37E
'ibm-1-1-not-wf-P04a-ibm04an04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an05.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2000
'ibm-1-1-not-wf-P04a-ibm04an05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an06.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2001
'ibm-1-1-not-wf-P04a-ibm04an06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an07.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2002
'ibm-1-1-not-wf-P04a-ibm04an07'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an08.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2005
'ibm-1-1-not-wf-P04a-ibm04an08'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an09.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x200B
'ibm-1-1-not-wf-P04a-ibm04an09'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an10.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x200E
'ibm-1-1-not-wf-P04a-ibm04an10'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an11.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2038
'ibm-1-1-not-wf-P04a-ibm04an11'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an12.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2041
'ibm-1-1-not-wf-P04a-ibm04an12'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an13.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2190
'ibm-1-1-not-wf-P04a-ibm04an13'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an14.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x23FF
'ibm-1-1-not-wf-P04a-ibm04an14'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an15.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x280F
'ibm-1-1-not-wf-P04a-ibm04an15'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an16.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2A00
'ibm-1-1-not-wf-P04a-ibm04an16'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an17.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFDD0
'ibm-1-1-not-wf-P04a-ibm04an17'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an18.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFDEF
'ibm-1-1-not-wf-P04a-ibm04an18'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an19.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2FFF
'ibm-1-1-not-wf-P04a-ibm04an19'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an20.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x3000
'ibm-1-1-not-wf-P04a-ibm04an20'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an21.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xD800
'ibm-1-1-not-wf-P04a-ibm04an21'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an22.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xD801
'ibm-1-1-not-wf-P04a-ibm04an22'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an23.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xDAFF
'ibm-1-1-not-wf-P04a-ibm04an23'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an24.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xDFFF
'ibm-1-1-not-wf-P04a-ibm04an24'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an25.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xEFFF
'ibm-1-1-not-wf-P04a-ibm04an25'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an26.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xF1FF
'ibm-1-1-not-wf-P04a-ibm04an26'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an27.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xF8FF
'ibm-1-1-not-wf-P04a-ibm04an27'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P04a-ibm04an28.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFFFFF
'ibm-1-1-not-wf-P04a-ibm04an28'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P04a/ibm04an28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n01.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x0B
'ibm-1-1-not-wf-P05-ibm05n01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n02.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x300
'ibm-1-1-not-wf-P05-ibm05n02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n03.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x36F
'ibm-1-1-not-wf-P05-ibm05n03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n04.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x203F
'ibm-1-1-not-wf-P05-ibm05n04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n05.xml
%% Description:
%%   Tests an element with an illegal Name containing #x2040
'ibm-1-1-not-wf-P05-ibm05n05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P05-ibm05n06.xml
%% Description:
%%   Tests an element with an illegal Name containing #0xB7
'ibm-1-1-not-wf-P05-ibm05n06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P05/ibm05n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n01.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external dtd 1.0. The external dtd contains the invalid XML1.1 but
%%   valid XML 1.0 character #x7F.
'ibm-1-1-not-wf-P77-ibm77n01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n02.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external dtd 1.0. The external dtd contains a comment with the
%%   invalid XML1.1 but valid XML 1.0 character #x80.
'ibm-1-1-not-wf-P77-ibm77n02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n03.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external dtd 1.0. The external dtd contains a PI with the invalid
%%   XML1.1 but valid XML 1.0 character #x9F.
'ibm-1-1-not-wf-P77-ibm77n03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n04.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external entity 1.0. The external entity the contains invalid XML1.1
%%   but valid XML 1.0 character #x89.
'ibm-1-1-not-wf-P77-ibm77n04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n05.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external entity 1.0. The external entity contains the invalid XML1.1
%%   but valid XML 1.0 character #x94.
'ibm-1-1-not-wf-P77-ibm77n05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n06.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and that of the
%%   external entity 1.0. The external entity contains the invalid XML1.1
%%   but valid XML 1.0 character #x9F.
'ibm-1-1-not-wf-P77-ibm77n06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n07.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a textDecl. The external entity contains the
%%   invalid XML1.1 but valid XML 1.0 character #x7F.
'ibm-1-1-not-wf-P77-ibm77n07'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n08.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a VersionNum in the textDecl. The external entity
%%   contains the invalid XML1.1 but valid XML 1.0 character #x9B.
'ibm-1-1-not-wf-P77-ibm77n08'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n09.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a textDecl. The external entity contains the
%%   invalid XML1.1 but valid XML 1.0 character #x8D.
'ibm-1-1-not-wf-P77-ibm77n09'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n10.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a VersionNum in the textDecl. The external entity
%%   contains the invalid XML 1.1 but valid XML 1.0 character #x84.
'ibm-1-1-not-wf-P77-ibm77n10'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n11.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a textDecl. The external entity contains the
%%   invalid XML 1.1 but valid XML 1.0 character #x88.
'ibm-1-1-not-wf-P77-ibm77n11'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n12.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 and the external dtd
%%   does not contain a textDecl. The external entity contains the
%%   invalid XML 1.1 but valid XML 1.0 character #x8E.
'ibm-1-1-not-wf-P77-ibm77n12'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n13.xml
%% Description:
%%   The VersionNum of the primary document entity is 1.0 and that of the
%%   external dtd is 1.0. The external dtd contains an external entity
%%   whose VersionNum is 1.1.
'ibm-1-1-not-wf-P77-ibm77n13'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n14.xml
%% Description:
%%   The VersionNum of the primary document entity is 1.1 and that of the
%%   external dtd is 1.0. The external dtd contains an element
%%   declaration with an invalid XML 1.1 and 1.0 name.
'ibm-1-1-not-wf-P77-ibm77n14'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n15.xml
%% Description:
%%   The VersionNum of the primary document entity is 1.1 and testDecl of
%%   the external dtd is absent. The external dtd contains an external
%%   entity whose VersionNum is 1.1 containing a valid XML1.0 but an
%%   invalid XML 1.1 character #x7F.
'ibm-1-1-not-wf-P77-ibm77n15'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n16.xml
%% Entities: general
%% Description:
%%   The VersionNum of the primary document entity is 1.0 and VersioNum
%%   of the external entity is absent. The replacement text of the entity
%%   contains an element followed by the valid XML 1.1 of line character
%%   NEL #x85 in its empty elem tag.
'ibm-1-1-not-wf-P77-ibm77n16'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n17.xml
%% Entities: general
%% Description:
%%   The VersionNum of the primary document entity is absent and that of
%%   the external entity is 1.0. The textDecl in the external entity
%%   contains an invalid XML1.0 but valid XML 1.1 enf of line character
%%   NEL #x85.
'ibm-1-1-not-wf-P77-ibm77n17'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n18.xml
%% Entities: general
%% Description:
%%   The VersionNum of the primary document entity is absent and that of
%%   the external entity is 1.0. The textDecl in the external entity
%%   contains an invalid XML1.0 but valid XML 1.1 of line character
%%   Unicode line separator #x2028.
'ibm-1-1-not-wf-P77-ibm77n18'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n19.xml
%% Entities: general
%% Description:
%%   The VersionNum of the primary document entity is 1.1 and that of the
%%   external dtd is absent. The external dtd contains an external entity
%%   whose VersionNum is absent and it contains a valid XML 1.0 but an
%%   invalid XML 1.1 character #x94.
'ibm-1-1-not-wf-P77-ibm77n19'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n20.xml
%% Entities: general
%% Description:
%%   The VersionNum of the primary document entity is 1.1 and that of the
%%   external dtd is 1.1. The external dtd contains an external entity
%%   whose VersionNum is absent and it contains a valid XML 1.0 but an
%%   invalid XML 1.1 character #x8F.
'ibm-1-1-not-wf-P77-ibm77n20'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-not-wf-P77-ibm77n21.xml
%% Entities: both
%% Description:
%%   The VersionNum of the primary document entity is 1.1 and the
%%   texlDecl of the external dtd is absent. The external dtd contains a
%%   reference to an external parameter entity whose VersionNum is absent
%%   from the textDecl and it contains an invalid XML 1.1 character #x8F.
'ibm-1-1-not-wf-P77-ibm77n21'(Config) ->
    run_test(Config, "ibm/xml-1.1", "not-wf/P77/ibm77n21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v01.xml
%% Description:
%%   This test case covers legal character ranges plus discrete legal
%%   characters for production 02 of the XML1.1 sepcification.
'ibm-1-1-valid-P02-ibm02v01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v02.xml
%% Description:
%%   This test case covers control characters x1 to x1F and x7F to x9F
%%   which should only appear as character references.
'ibm-1-1-valid-P02-ibm02v02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v02.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v03.xml
%% Description:
%%   This test case covers control characters x1 to x1F and x7F to x9F
%%   which appear as character references as an entity's replacement
%%   text.
'ibm-1-1-valid-P02-ibm02v03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v04.xml
%% Description:
%%   This test case contains embeded whitespace characters some form the
%%   range 1 - 1F.
'ibm-1-1-valid-P02-ibm02v04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v05.xml
%% Description:
%%   This test case contains valid char references that match the char
%%   production.
'ibm-1-1-valid-P02-ibm02v05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P02-ibm02v06.xml
%% Entities: general
%% Description:
%%   This test case contains valid char references in the CDATA section,
%%   comment and processing instruction of an external entity that match
%%   the char production.
'ibm-1-1-valid-P02-ibm02v06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P02/ibm02v06.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v01.xml
%% Entities: general
%% Output: valid/P03/out/ibm03v01.xml
%% Description:
%%   The two character sequence #x0D #x85 in an external entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v01'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v01.xml", "valid", "valid/P03/out/ibm03v01.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v02.xml
%% Entities: general
%% Output: valid/P03/out/ibm03v02.xml
%% Description:
%%   The single character sequence #x85 in an external entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v02'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v02.xml", "valid", "valid/P03/out/ibm03v02.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v03.xml
%% Entities: general
%% Output: valid/P03/out/ibm03v03.xml
%% Description:
%%   The two character sequence #x0D #x85 in an external entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v03'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v03.xml", "valid", "valid/P03/out/ibm03v03.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v04.xml
%% Entities: general
%% Output: valid/P03/out/ibm03v04.xml
%% Description:
%%   The single character sequence #x85 in an external entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v04'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v04.xml", "valid", "valid/P03/out/ibm03v04.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v05.xml
%% Output: valid/P03/out/ibm03v05.xml
%% Description:
%%   The two character sequence #x0D #x85 in a document entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v05'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v05.xml", "valid", "valid/P03/out/ibm03v05.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v06.xml
%% Output: valid/P03/out/ibm03v06.xml
%% Description:
%%   The single character sequence #x85 in a document entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v06'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v06.xml", "valid", "valid/P03/out/ibm03v06.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v07.xml
%% Output: valid/P03/out/ibm03v07.xml
%% Description:
%%   The single character sequence #x2028 in a document entity must be
%%   normalized to a single newline.
'ibm-1-1-valid-P03-ibm03v07'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v07.xml", "valid", "valid/P03/out/ibm03v07.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v08.xml
%% Output: valid/P03/out/ibm03v08.xml
%% Description:
%%   The single character sequence #x85 in the XMLDecl must be normalized
%%   to a single newline.
'ibm-1-1-valid-P03-ibm03v08'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v08.xml", "valid", "valid/P03/out/ibm03v08.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P03-ibm03v09.xml
%% Entities: general
%% Output: valid/P03/out/ibm03v09.xml
%% Description:
%%   The single character sequence #x2028 in the XMLDecl must be
%%   normalized to a single newline. (This test is questionable)
'ibm-1-1-valid-P03-ibm03v09'(Config) ->
    run_test(
        Config, "ibm/xml-1.1", "valid/P03/ibm03v09.xml", "valid", "valid/P03/out/ibm03v09.xml"
    ).

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P04-ibm04v01.xml
%% Description:
%%   This test case covers legal NameStartChars character ranges plus
%%   discrete legal characters for production 04.
'ibm-1-1-valid-P04-ibm04v01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P04/ibm04v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P04-ibm04av01.xml
%% Description:
%%   This test case covers legal NameChars character ranges plus discrete
%%   legal characters for production 04a.
'ibm-1-1-valid-P04-ibm04av01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P04a/ibm04av01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P05-ibm05v01.xml
%% Description:
%%   This test case covers legal Element Names as per production 5.
'ibm-1-1-valid-P05-ibm05v01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P05/ibm05v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P05-ibm05v02.xml
%% Description:
%%   This test case covers legal PITarget (Names) as per production 5.
'ibm-1-1-valid-P05-ibm05v02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P05/ibm05v02.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P05-ibm05v03.xml
%% Description:
%%   This test case covers legal Attribute (Names) as per production 5.
'ibm-1-1-valid-P05-ibm05v03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P05/ibm05v03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P05-ibm05v04.xml
%% Description:
%%   This test case covers legal ID/IDREF (Names) as per production 5.
'ibm-1-1-valid-P05-ibm05v04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P05/ibm05v04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P05-ibm05v05.xml
%% Description:
%%   This test case covers legal ENTITY (Names) as per production 5.
'ibm-1-1-valid-P05-ibm05v05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P05/ibm05v05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P047-ibm07v01.xml
%% Description:
%%   This test case covers legal NMTOKEN Name character ranges plus
%%   discrete legal characters for production 7.
'ibm-1-1-valid-P047-ibm07v01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P07/ibm07v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v01.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external DTD is 1.0. The character #xC0 which is a valid XML
%%   1.1 but an invalid XML 1.0 character is present in both documents.
'ibm-1-1-valid-P77-ibm77v01'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v02.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external DTD is 1.0. The character #x1FFF which is a valid
%%   XML 1.1 but an invalid XML 1.0 character is present in both
%%   documents.
'ibm-1-1-valid-P77-ibm77v02'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v02.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v03.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external DTD is 1.0. The character #xF901 which is a valid
%%   XML 1.1 but an invalid XML 1.0 character is present in both
%%   documents.
'ibm-1-1-valid-P77-ibm77v03'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v04.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external entity is 1.0. The character #xD6 which is a valid
%%   XML 1.1 but an invalid XML 1.0 character is present in both
%%   documents.
'ibm-1-1-valid-P77-ibm77v04'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v05.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external entity is 1.0. The character #x1FFF which is a valid
%%   XML 1.1 but an invalid XML 1.0 character is present in both
%%   documents.
'ibm-1-1-valid-P77-ibm77v05'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v06.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 whereas the VersionNum
%%   of the external entity is 1.0. The character #xF901 which is a valid
%%   XML 1.1 but an invalid XML 1.0 character is present in both
%%   documents.
'ibm-1-1-valid-P77-ibm77v06'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v06.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v07.xml
%% Description:
%%   The VersionNum of the document and external dtd is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #xD8.
'ibm-1-1-valid-P77-ibm77v07'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v07.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v08.xml
%% Description:
%%   The VersionNum of the document and external dtd is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #x1FFF.
'ibm-1-1-valid-P77-ibm77v08'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v08.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v09.xml
%% Description:
%%   The VersionNum of the document and external dtd is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #xF901.
'ibm-1-1-valid-P77-ibm77v09'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v09.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v10.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external entity is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #xF6.
'ibm-1-1-valid-P77-ibm77v10'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v10.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v11.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external entity is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #x1FFF.
'ibm-1-1-valid-P77-ibm77v11'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v11.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v12.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external entity is 1.1 and both
%%   contain the valid XML1.1 but invalid XML1.0 character #xF901.
'ibm-1-1-valid-P77-ibm77v12'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v12.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v13.xml
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external dtd
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #xF8.
'ibm-1-1-valid-P77-ibm77v13'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v13.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v14.xml
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external dtd
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #x1FFF.
'ibm-1-1-valid-P77-ibm77v14'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v14.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v15.xml
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external dtd
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #xF901.
'ibm-1-1-valid-P77-ibm77v15'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v15.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v16.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external entity
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #x2FF.
'ibm-1-1-valid-P77-ibm77v16'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v17.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external entity
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #x1FFF.
'ibm-1-1-valid-P77-ibm77v17'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v17.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v18.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document entity is 1.1 but the external entity
%%   does not contain a textDecl and both contain the valid XML1.1 but
%%   invalid XML1.0 character #xF901.
'ibm-1-1-valid-P77-ibm77v18'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v18.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v19.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external dtd is 1.1. The
%%   replacement text of an entity declared in the external DTD contains
%%   a reference to the character #x7F. This entity is not referenced in
%%   the document entity.
'ibm-1-1-valid-P77-ibm77v19'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v19.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v20.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external dtd is 1.1. The
%%   replacement text of an entity declared in the external DTD contains
%%   a reference to the character #x80. This entity is not referenced in
%%   the document entity.
'ibm-1-1-valid-P77-ibm77v20'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v20.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v21.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and external dtd is 1.1. The
%%   replacement text of an entity declared in the external DTD contains
%%   a reference to the character #x9F. This entity is not referenced in
%%   the document entity.
'ibm-1-1-valid-P77-ibm77v21'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v21.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v22.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and the external entity is 1.1. The
%%   entity contains a reference to the character #x7F.
'ibm-1-1-valid-P77-ibm77v22'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v22.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v23.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and the external entity is 1.1. The
%%   entity contains a reference to the character #x80.
'ibm-1-1-valid-P77-ibm77v23'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v23.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v24.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document and the external entity is 1.1. The
%%   entity contains a reference to the character #x9F.
'ibm-1-1-valid-P77-ibm77v24'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v24.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v25.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external DTD. The replacement text of an entity declared in the
%%   external DTD contains a reference to the character #x7F, #x8F. This
%%   entity is not referenced in the document entity.
'ibm-1-1-valid-P77-ibm77v25'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v25.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v26.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external DTD. The replacement text of an entity declared in the
%%   external DTD contains a reference to the character #x80, #x90. This
%%   entity is not referenced in the document entity.
'ibm-1-1-valid-P77-ibm77v26'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v26.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v27.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external DTD. The replacement text of an entity declared in the
%%   external DTD contains a reference to the character #x81, #x9F. This
%%   entity is not referenced in the document entity.
'ibm-1-1-valid-P77-ibm77v27'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v27.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v28.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external entity. The replacement text of an entity declared in
%%   the external DTD contains a reference to the character #x7F, #x80,
%%   #x9F.
'ibm-1-1-valid-P77-ibm77v28'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v28.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v29.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external entity. The replacement text of an entity declared in
%%   the external DTD contains a reference to the character #x85, #x8F.
'ibm-1-1-valid-P77-ibm77v29'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v29.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-1-1-valid-P77-ibm77v30.xml
%% Entities: general
%% Description:
%%   The VersionNum of the document is 1.1 and the textDecl is missing in
%%   the external entity. The replacement text of an entity declared in
%%   the external DTD contains a reference to the character #x1, #x7F.
'ibm-1-1-valid-P77-ibm77v30'(Config) ->
    run_test(Config, "ibm/xml-1.1", "valid/P77/ibm77v30.xml", "valid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-e2e-2a
%% Description:
%%   Duplicate token in enumerated attribute declaration
'rmt-e2e-2a'(Config) -> run_test(Config, "eduni/errata-2e", "E2a.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-2b
%% Description:
%%   Duplicate token in NOTATION attribute declaration
'rmt-e2e-2b'(Config) -> run_test(Config, "eduni/errata-2e", "E2b.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-9a
%% Description:
%%   An unused attribute default need only be syntactically correct
'rmt-e2e-9a'(Config) -> run_test(Config, "eduni/errata-2e", "E9a.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-9b
%% Description:
%%   An attribute default must be syntactically correct even if unused
'rmt-e2e-9b'(Config) -> run_test(Config, "eduni/errata-2e", "E9b.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-14
%% Entities: parameter
%% Description:
%%   Declarations mis-nested wrt parameter entities are just validity
%%   errors (but note that some parsers treat some such errors as fatal)
'rmt-e2e-14'(Config) -> run_test(Config, "eduni/errata-2e", "E14.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15a
%% Description:
%%   Empty content can't contain an entity reference
'rmt-e2e-15a'(Config) -> run_test(Config, "eduni/errata-2e", "E15a.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15b
%% Description:
%%   Empty content can't contain a comment
'rmt-e2e-15b'(Config) -> run_test(Config, "eduni/errata-2e", "E15b.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15c
%% Description:
%%   Empty content can't contain a PI
'rmt-e2e-15c'(Config) -> run_test(Config, "eduni/errata-2e", "E15c.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15d
%% Description:
%%   Empty content can't contain whitespace
'rmt-e2e-15d'(Config) -> run_test(Config, "eduni/errata-2e", "E15d.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15e
%% Description:
%%   Element content can contain entity reference if replacement text is
%%   whitespace
'rmt-e2e-15e'(Config) -> run_test(Config, "eduni/errata-2e", "E15e.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15f
%% Description:
%%   Element content can contain entity reference if replacement text is
%%   whitespace, even if it came from a character reference in the
%%   literal entity value
'rmt-e2e-15f'(Config) -> run_test(Config, "eduni/errata-2e", "E15f.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15g
%% Description:
%%   Element content can't contain character reference to whitespace
'rmt-e2e-15g'(Config) -> run_test(Config, "eduni/errata-2e", "E15g.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15h
%% Description:
%%   Element content can't contain entity reference if replacement text
%%   is character reference to whitespace
'rmt-e2e-15h'(Config) -> run_test(Config, "eduni/errata-2e", "E15h.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15i
%% Description:
%%   Element content can contain a comment
'rmt-e2e-15i'(Config) -> run_test(Config, "eduni/errata-2e", "E15i.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15j
%% Description:
%%   Element content can contain a PI
'rmt-e2e-15j'(Config) -> run_test(Config, "eduni/errata-2e", "E15j.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15k
%% Description:
%%   Mixed content can contain a comment
'rmt-e2e-15k'(Config) -> run_test(Config, "eduni/errata-2e", "E15k.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-15l
%% Description:
%%   Mixed content can contain a PI
'rmt-e2e-15l'(Config) -> run_test(Config, "eduni/errata-2e", "E15l.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-18
%% Entities: both
%% Output: out/E18.xml
%% Description:
%%   External entity containing start of entity declaration is base URI
%%   for system identifier
'rmt-e2e-18'(Config) -> run_test(Config, "eduni/errata-2e", "E18.xml", "valid", "out/E18.xml").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-19
%% Entities: parameter
%% Output: out/E19.xml
%% Description:
%%   Parameter entities and character references are included-in-literal,
%%   but general entities are bypassed.
'rmt-e2e-19'(Config) -> run_test(Config, "eduni/errata-2e", "E19.xml", "valid", "out/E19.xml").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-20
%% Description:
%%   Tokens, after normalization, must be separated by space, not other
%%   whitespace characters
'rmt-e2e-20'(Config) -> run_test(Config, "eduni/errata-2e", "E20.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-22
%% Description:
%%   UTF-8 entities may start with a BOM
'rmt-e2e-22'(Config) -> run_test(Config, "eduni/errata-2e", "E22.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-24
%% Description:
%%   Either the built-in entity or a character reference can be used to
%%   represent greater-than after two close-square-brackets
'rmt-e2e-24'(Config) -> run_test(Config, "eduni/errata-2e", "E24.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-27
%% Description:
%%   Contains an irregular UTF-8 sequence (i.e. a surrogate pair)
'rmt-e2e-27'(Config) -> run_test(Config, "eduni/errata-2e", "E27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-29
%% Description:
%%   Three-letter language codes are allowed
'rmt-e2e-29'(Config) -> run_test(Config, "eduni/errata-2e", "E29.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-34
%% Description:
%%   A non-deterministic content model is an error even if the element
%%   type is not used.
'rmt-e2e-34'(Config) -> run_test(Config, "eduni/errata-2e", "E34.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-36
%% Entities: parameter
%% Description:
%%   An external ATTLIST declaration does not make a document
%%   non-standalone if the normalization would have been the same without
%%   the declaration
'rmt-e2e-36'(Config) -> run_test(Config, "eduni/errata-2e", "E36.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-38
%% Entities: general
%% Description:
%%   XML 1.0 document refers to 1.1 entity

%% run_test(Config, "eduni/errata-2e", "E38.xml", "not-wf").
'rmt-e2e-38'(_Config) ->
    {skip, "E10 v4e: allow parsers to attempt forward-compatible processing of post-1.0 documents"}.

%%----------------------------------------------------------------------
%% ID: rmt-e2e-41
%% Description:
%%   An xml:lang attribute may be empty
'rmt-e2e-41'(Config) -> run_test(Config, "eduni/errata-2e", "E41.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-48
%% Description:
%%   ANY content allows character data
'rmt-e2e-48'(Config) -> run_test(Config, "eduni/errata-2e", "E48.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-50
%% Description:
%%   All line-ends are normalized, even those not passed to the
%%   application. NB this can only be tested effectively in XML 1.1,
%%   since CR is in the S production; in 1.1 we can use NEL which isn't.

%% run_test(Config, "eduni/errata-2e", "E50.xml", "valid").
'rmt-e2e-50'(_Config) -> {skip, "XML 1.1 EOL Char"}.

%%----------------------------------------------------------------------
%% ID: rmt-e2e-55
%% Description:
%%   A reference to an unparsed entity in an entity value is an error
%%   rather than forbidden (unless the entity is referenced, of course)
'rmt-e2e-55'(Config) -> run_test(Config, "eduni/errata-2e", "E55.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-57
%% Description:
%%   A value other than preserve or default for xml:space is an error
'rmt-e2e-57'(Config) -> run_test(Config, "eduni/errata-2e", "E57.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-60
%% Entities: parameter
%% Description:
%%   Conditional sections are allowed in external parameter entities
%%   referred to from the internal subset.
'rmt-e2e-60'(Config) -> run_test(Config, "eduni/errata-2e", "E60.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e2e-61
%% Description:
%%   (From John Cowan) An encoding declaration in ASCII specifying an
%%   encoding that is not compatible with ASCII (so the document is not
%%   in its declared encoding). It should generate a fatal error.

%% run_test(Config, "eduni/errata-2e", "E61.xml", "not-wf").
'rmt-e2e-61'(_Config) -> {skip, "Non-matched file encoding and declaration, app only sees UTF-8"}.

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-001
%% Entities: parameter
%% Description:
%%   External subset has later version number
'rmt-001'(Config) -> run_test(Config, "eduni/xml-1.1", "001.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-002
%% Entities: parameter
%% Description:
%%   External PE has later version number
'rmt-002'(Config) -> run_test(Config, "eduni/xml-1.1", "002.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-003
%% Entities: general
%% Description:
%%   External general entity has later version number
'rmt-003'(Config) -> run_test(Config, "eduni/xml-1.1", "003.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-004
%% Entities: general
%% Description:
%%   External general entity has later version number (no decl means 1.0)
'rmt-004'(Config) -> run_test(Config, "eduni/xml-1.1", "004.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-005
%% Entities: general
%% Description:
%%   Indirect external general entity has later version number
'rmt-005'(Config) -> run_test(Config, "eduni/xml-1.1", "005.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-006
%% Entities: general
%% Output: out/006.xml
%% Description:
%%   Second-level external general entity has later version number than
%%   first-level, but not later than document, so not an error.
'rmt-006'(Config) -> run_test(Config, "eduni/xml-1.1", "006.xml", "valid", "out/006.xml").

%%----------------------------------------------------------------------
%% ID: rmt-007
%% Output: out/007.xml
%% Description:
%%   A vanilla XML 1.1 document
'rmt-007'(Config) -> run_test(Config, "eduni/xml-1.1", "007.xml", "valid", "out/007.xml").

%%----------------------------------------------------------------------
%% ID: rmt-008
%% Description:
%%   an implausibly-versioned document
'rmt-008'(Config) -> run_test(Config, "eduni/xml-1.1", "008.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-009
%% Entities: general
%% Description:
%%   External general entity has implausible version number
'rmt-009'(Config) -> run_test(Config, "eduni/xml-1.1", "009.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-010
%% Output: out/010.xml
%% Description:
%%   Contains a C1 control, legal in XML 1.0, illegal in XML 1.1
'rmt-010'(Config) -> run_test(Config, "eduni/xml-1.1", "010.xml", "valid", "out/010.xml").

%%----------------------------------------------------------------------
%% ID: rmt-011
%% Description:
%%   Contains a C1 control, legal in XML 1.0, illegal in XML 1.1
'rmt-011'(Config) -> run_test(Config, "eduni/xml-1.1", "011.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-012
%% Output: out/012.xml
%% Description:
%%   Contains a DEL, legal in XML 1.0, illegal in XML 1.1
'rmt-012'(Config) -> run_test(Config, "eduni/xml-1.1", "012.xml", "valid", "out/012.xml").

%%----------------------------------------------------------------------
%% ID: rmt-013
%% Description:
%%   Contains a DEL, legal in XML 1.0, illegal in XML 1.1
'rmt-013'(Config) -> run_test(Config, "eduni/xml-1.1", "013.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-015
%% Output: out/015.xml
%% Description:
%%   Has a "long s" in a name, legal in XML 1.1, illegal in XML 1.0 thru
%%   4th edition
'rmt-015'(Config) -> run_test(Config, "eduni/xml-1.1", "015.xml", "invalid", "out/015.xml").

%%----------------------------------------------------------------------
%% ID: rmt-017
%% Output: out/017.xml
%% Description:
%%   Has a Byzantine Musical Symbol Kratimata in a name, legal in XML
%%   1.1, illegal in XML 1.0 thru 4th edition
'rmt-017'(Config) -> run_test(Config, "eduni/xml-1.1", "017.xml", "invalid", "out/017.xml").

%%----------------------------------------------------------------------
%% ID: rmt-018
%% Output: out/018.xml
%% Description:
%%   Has the last legal namechar in XML 1.1, illegal in XML 1.0 thru 4th
%%   edition
'rmt-018'(Config) -> run_test(Config, "eduni/xml-1.1", "018.xml", "invalid", "out/018.xml").

%%----------------------------------------------------------------------
%% ID: rmt-020
%% Description:
%%   Has the first character after the last legal namechar in XML 1.1,
%%   illegal in both XML 1.0 and 1.1
'rmt-020'(Config) -> run_test(Config, "eduni/xml-1.1", "020.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-021
%% Description:
%%   Has the first character after the last legal namechar in XML 1.1,
%%   illegal in both XML 1.0 and 1.1
'rmt-021'(Config) -> run_test(Config, "eduni/xml-1.1", "021.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-022
%% Output: out/022.xml
%% Description:
%%   Has a NEL character; legal in both XML 1.0 and 1.1, but different
%%   canonical output because of normalization in 1.1
'rmt-022'(Config) -> run_test(Config, "eduni/xml-1.1", "022.xml", "valid", "out/022.xml").

%%----------------------------------------------------------------------
%% ID: rmt-023
%% Output: out/023.xml
%% Description:
%%   Has a NEL character; legal in both XML 1.0 and 1.1, but different
%%   canonical output because of normalization in 1.1
'rmt-023'(Config) -> run_test(Config, "eduni/xml-1.1", "023.xml", "valid", "out/023.xml").

%%----------------------------------------------------------------------
%% ID: rmt-024
%% Output: out/024.xml
%% Description:
%%   Has an LSEP character; legal in both XML 1.0 and 1.1, but different
%%   canonical output because of normalization in 1.1
'rmt-024'(Config) -> run_test(Config, "eduni/xml-1.1", "024.xml", "valid", "out/024.xml").

%%----------------------------------------------------------------------
%% ID: rmt-025
%% Output: out/025.xml
%% Description:
%%   Has an LSEP character; legal in both XML 1.0 and 1.1, but different
%%   canonical output because of normalization in 1.1
'rmt-025'(Config) -> run_test(Config, "eduni/xml-1.1", "025.xml", "valid", "out/025.xml").

%%----------------------------------------------------------------------
%% ID: rmt-026
%% Output: out/026.xml
%% Description:
%%   Has CR-NEL; legal in both XML 1.0 and 1.1, but different canonical
%%   output because of normalization in 1.1
'rmt-026'(Config) -> run_test(Config, "eduni/xml-1.1", "026.xml", "valid", "out/026.xml").

%%----------------------------------------------------------------------
%% ID: rmt-027
%% Output: out/027.xml
%% Description:
%%   Has CR-NEL; legal in both XML 1.0 and 1.1, but different canonical
%%   output because of normalization in 1.1
'rmt-027'(Config) -> run_test(Config, "eduni/xml-1.1", "027.xml", "valid", "out/027.xml").

%%----------------------------------------------------------------------
%% ID: rmt-028
%% Output: out/028.xml
%% Description:
%%   Has CR-LSEP; legal in both XML 1.0 and 1.1, but different canonical
%%   output because of normalization in 1.1. Note that CR and LSEP are
%%   not combined into a single LF
'rmt-028'(Config) -> run_test(Config, "eduni/xml-1.1", "028.xml", "valid", "out/028.xml").

%%----------------------------------------------------------------------
%% ID: rmt-029
%% Output: out/029.xml
%% Description:
%%   Has CR-LSEP; legal in both XML 1.0 and 1.1, but different canonical
%%   output because of normalization in 1.1
'rmt-029'(Config) -> run_test(Config, "eduni/xml-1.1", "029.xml", "valid", "out/029.xml").

%%----------------------------------------------------------------------
%% ID: rmt-030
%% Output: out/030.xml
%% Description:
%%   Has a NEL character in an NMTOKENS attribute; well-formed in both
%%   XML 1.0 and 1.1, but valid only in 1.1
'rmt-030'(Config) -> run_test(Config, "eduni/xml-1.1", "030.xml", "invalid", "out/030.xml").

%%----------------------------------------------------------------------
%% ID: rmt-031
%% Output: out/031.xml
%% Description:
%%   Has a NEL character in an NMTOKENS attribute; well-formed in both
%%   XML 1.0 and 1.1, but valid only in 1.1
'rmt-031'(Config) -> run_test(Config, "eduni/xml-1.1", "031.xml", "valid", "out/031.xml").

%%----------------------------------------------------------------------
%% ID: rmt-032
%% Output: out/032.xml
%% Description:
%%   Has an LSEP character in an NMTOKENS attribute; well-formed in both
%%   XML 1.0 and 1.1, but valid only in 1.1
'rmt-032'(Config) -> run_test(Config, "eduni/xml-1.1", "032.xml", "invalid", "out/032.xml").

%%----------------------------------------------------------------------
%% ID: rmt-033
%% Output: out/033.xml
%% Description:
%%   Has an LSEP character in an NMTOKENS attribute; well-formed in both
%%   XML 1.0 and 1.1, but valid only in 1.1
'rmt-033'(Config) -> run_test(Config, "eduni/xml-1.1", "033.xml", "valid", "out/033.xml").

%%----------------------------------------------------------------------
%% ID: rmt-034
%% Output: out/034.xml
%% Description:
%%   Has an NMTOKENS attribute containing a CR character that comes from
%%   a character reference in an internal entity. Because CR is in the S
%%   production, this is valid in both XML 1.0 and 1.1.
'rmt-034'(Config) -> run_test(Config, "eduni/xml-1.1", "034.xml", "valid", "out/034.xml").

%%----------------------------------------------------------------------
%% ID: rmt-035
%% Output: out/035.xml
%% Description:
%%   Has an NMTOKENS attribute containing a CR character that comes from
%%   a character reference in an internal entity. Because CR is in the S
%%   production, this is valid in both XML 1.0 and 1.1.
'rmt-035'(Config) -> run_test(Config, "eduni/xml-1.1", "035.xml", "valid", "out/035.xml").

%%----------------------------------------------------------------------
%% ID: rmt-036
%% Output: out/036.xml
%% Description:
%%   Has an NMTOKENS attribute containing a NEL character that comes from
%%   a character reference in an internal entity. Because NEL is not in
%%   the S production (even though real NELs are converted to LF on
%%   input), this is invalid in both XML 1.0 and 1.1.
'rmt-036'(Config) -> run_test(Config, "eduni/xml-1.1", "036.xml", "invalid", "out/036.xml").

%%----------------------------------------------------------------------
%% ID: rmt-037
%% Output: out/037.xml
%% Description:
%%   Has an NMTOKENS attribute containing a NEL character that comes from
%%   a character reference in an internal entity. Because NEL is not in
%%   the S production (even though real NELs are converted to LF on
%%   input), this is invalid in both XML 1.0 and 1.1.
'rmt-037'(Config) -> run_test(Config, "eduni/xml-1.1", "037.xml", "invalid", "out/037.xml").

%%----------------------------------------------------------------------
%% ID: rmt-038
%% Description:
%%   Contains a C0 control character (form-feed), illegal in both XML 1.0
%%   and 1.1
'rmt-038'(Config) -> run_test(Config, "eduni/xml-1.1", "038.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-039
%% Description:
%%   Contains a C0 control character (form-feed), illegal in both XML 1.0
%%   and 1.1
'rmt-039'(Config) -> run_test(Config, "eduni/xml-1.1", "039.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-040
%% Output: out/040.xml
%% Description:
%%   Contains a C1 control character (partial line up), legal in XML 1.0
%%   but not 1.1
'rmt-040'(Config) -> run_test(Config, "eduni/xml-1.1", "040.xml", "valid", "out/040.xml").

%%----------------------------------------------------------------------
%% ID: rmt-041
%% Description:
%%   Contains a C1 control character (partial line up), legal in XML 1.0
%%   but not 1.1
'rmt-041'(Config) -> run_test(Config, "eduni/xml-1.1", "041.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-042
%% Description:
%%   Contains a character reference to a C0 control character
%%   (form-feed), legal in XML 1.1 but not 1.0
'rmt-042'(Config) -> run_test(Config, "eduni/xml-1.1", "042.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-043
%% Output: out/043.xml
%% Description:
%%   Contains a character reference to a C0 control character
%%   (form-feed), legal in XML 1.1 but not 1.0
'rmt-043'(Config) -> run_test(Config, "eduni/xml-1.1", "043.xml", "valid", "out/043.xml").

%%----------------------------------------------------------------------
%% ID: rmt-044
%% Output: out/044.xml
%% Description:
%%   Contains a character reference to a C1 control character (partial
%%   line up), legal in both XML 1.0 and 1.1 (but for different reasons)
'rmt-044'(Config) -> run_test(Config, "eduni/xml-1.1", "044.xml", "valid", "out/044.xml").

%%----------------------------------------------------------------------
%% ID: rmt-045
%% Output: out/045.xml
%% Description:
%%   Contains a character reference to a C1 control character (partial
%%   line up), legal in both XML 1.0 and 1.1 (but for different reasons)
'rmt-045'(Config) -> run_test(Config, "eduni/xml-1.1", "045.xml", "valid", "out/045.xml").

%%----------------------------------------------------------------------
%% ID: rmt-046
%% Output: out/046.xml
%% Description:
%%   Has a NEL character in element content whitespace; well-formed in
%%   both XML 1.0 and 1.1, but valid only in 1.1
'rmt-046'(Config) -> run_test(Config, "eduni/xml-1.1", "046.xml", "invalid", "out/046.xml").

%%----------------------------------------------------------------------
%% ID: rmt-047
%% Output: out/047.xml
%% Description:
%%   Has a NEL character in element content whitespace; well-formed in
%%   both XML 1.0 and 1.1, but valid only in 1.1
'rmt-047'(Config) -> run_test(Config, "eduni/xml-1.1", "047.xml", "valid", "out/047.xml").

%%----------------------------------------------------------------------
%% ID: rmt-048
%% Output: out/048.xml
%% Description:
%%   Has an LSEP character in element content whitespace; well-formed in
%%   both XML 1.0 and 1.1, but valid only in 1.1
'rmt-048'(Config) -> run_test(Config, "eduni/xml-1.1", "048.xml", "invalid", "out/048.xml").

%%----------------------------------------------------------------------
%% ID: rmt-049
%% Output: out/049.xml
%% Description:
%%   has an LSEP character in element content whitespace; well-formed in
%%   both XML 1.0 and 1.1, but valid only in 1.1
'rmt-049'(Config) -> run_test(Config, "eduni/xml-1.1", "049.xml", "valid", "out/049.xml").

%%----------------------------------------------------------------------
%% ID: rmt-050
%% Output: out/050.xml
%% Description:
%%   Has element content whitespace containing a CR character that comes
%%   from a character reference in an internal entity. Because CR is in
%%   the S production, this is valid in both XML 1.0 and 1.1.
'rmt-050'(Config) -> run_test(Config, "eduni/xml-1.1", "050.xml", "valid", "out/050.xml").

%%----------------------------------------------------------------------
%% ID: rmt-051
%% Output: out/051.xml
%% Description:
%%   Has element content whitespace containing a CR character that comes
%%   from a character reference in an internal entity. Because CR is in
%%   the S production, this is valid in both XML 1.0 and 1.1.
'rmt-051'(Config) -> run_test(Config, "eduni/xml-1.1", "051.xml", "valid", "out/051.xml").

%%----------------------------------------------------------------------
%% ID: rmt-052
%% Output: out/052.xml
%% Description:
%%   Has element content whitespace containing a NEL character that comes
%%   from a character reference in an internal entity. Because NEL is not
%%   in the S production (even though real NELs are converted to LF on
%%   input), this is invalid in both XML 1.0 and 1.1.
'rmt-052'(Config) -> run_test(Config, "eduni/xml-1.1", "052.xml", "invalid", "out/052.xml").

%%----------------------------------------------------------------------
%% ID: rmt-053
%% Output: out/053.xml
%% Description:
%%   Has element content whitespace containing a NEL character that comes
%%   from a character reference in an internal entity. Because NEL is not
%%   in the S production (even though real NELs are converted to LF on
%%   input), this is invalid in both XML 1.0 and 1.1.
'rmt-053'(Config) -> run_test(Config, "eduni/xml-1.1", "053.xml", "invalid", "out/053.xml").

%%----------------------------------------------------------------------
%% ID: rmt-054
%% Output: out/054.xml
%% Description:
%%   Contains a character reference to a C0 control character (form-feed)
%%   in an entity value. This will be legal (in XML 1.1) when the entity
%%   declaration is parsed, but what about when it is used? According to
%%   the grammar in the CR spec, it should be illegal (because the
%%   replacement text must match "content"), but this is probably not
%%   intended. This will be fixed in the PR version.
'rmt-054'(Config) -> run_test(Config, "eduni/xml-1.1", "054.xml", "valid", "out/054.xml").

%%----------------------------------------------------------------------
%% ID: rmt-055
%% Description:
%%   Has a Latin-1 NEL in the XML declaration (to be made an error in PR)
'rmt-055'(Config) -> run_test(Config, "eduni/xml-1.1", "055.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-056
%% Description:
%%   Has a UTF-8 NEL in the XML declaration (to be made an error in PR)
'rmt-056'(Config) -> run_test(Config, "eduni/xml-1.1", "056.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-057
%% Description:
%%   Has a UTF-8 LSEP in the XML declaration (to be made an error in PR)
'rmt-057'(Config) -> run_test(Config, "eduni/xml-1.1", "057.xml", "error").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-ns10-001
%% Description:
%%   Namespace name test: a perfectly good http URI
'rmt-ns10-001'(Config) -> run_test(Config, "eduni/namespaces/1.0", "001.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-002
%% Description:
%%   Namespace name test: a syntactically plausible URI with a fictitious
%%   scheme
'rmt-ns10-002'(Config) -> run_test(Config, "eduni/namespaces/1.0", "002.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-003
%% Description:
%%   Namespace name test: a perfectly good http URI with a fragment
'rmt-ns10-003'(Config) -> run_test(Config, "eduni/namespaces/1.0", "003.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-004
%% Description:
%%   Namespace name test: a relative URI (deprecated)

%% run_test(Config, "eduni/namespaces/1.0", "004.xml", "error").
'rmt-ns10-004'(_Config) -> {skip, "Namespace name test: a relative URI (deprecated)"}.

%%----------------------------------------------------------------------
%% ID: rmt-ns10-005
%% Description:
%%   Namespace name test: a same-document relative URI (deprecated)

%% run_test(Config, "eduni/namespaces/1.0", "005.xml", "error").
'rmt-ns10-005'(_Config) -> {skip, "Namespace name test: a same-document relative URI (deprecated)"}.

%%----------------------------------------------------------------------
%% ID: rmt-ns10-006
%% Description:
%%   Namespace name test: an http IRI that is not a URI
'rmt-ns10-006'(Config) -> run_test(Config, "eduni/namespaces/1.0", "006.xml", "error").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-007
%% Description:
%%   Namespace inequality test: different capitalization
'rmt-ns10-007'(Config) -> run_test(Config, "eduni/namespaces/1.0", "007.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-008
%% Description:
%%   Namespace inequality test: different escaping
'rmt-ns10-008'(Config) -> run_test(Config, "eduni/namespaces/1.0", "008.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-009
%% Description:
%%   Namespace equality test: plain repetition
'rmt-ns10-009'(Config) -> run_test(Config, "eduni/namespaces/1.0", "009.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-010
%% Description:
%%   Namespace equality test: use of character reference
'rmt-ns10-010'(Config) -> run_test(Config, "eduni/namespaces/1.0", "010.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-011
%% Description:
%%   Namespace equality test: use of entity reference
'rmt-ns10-011'(Config) -> run_test(Config, "eduni/namespaces/1.0", "011.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-012
%% Description:
%%   Namespace inequality test: equal after attribute value normalization
'rmt-ns10-012'(Config) -> run_test(Config, "eduni/namespaces/1.0", "012.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-013
%% Description:
%%   Bad QName syntax: multiple colons
'rmt-ns10-013'(Config) -> run_test(Config, "eduni/namespaces/1.0", "013.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-014
%% Description:
%%   Bad QName syntax: colon at end
'rmt-ns10-014'(Config) -> run_test(Config, "eduni/namespaces/1.0", "014.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-015
%% Description:
%%   Bad QName syntax: colon at start
'rmt-ns10-015'(Config) -> run_test(Config, "eduni/namespaces/1.0", "015.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-016
%% Description:
%%   Bad QName syntax: xmlns:
'rmt-ns10-016'(Config) -> run_test(Config, "eduni/namespaces/1.0", "016.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-017
%% Description:
%%   Simple legal case: no namespaces
'rmt-ns10-017'(Config) -> run_test(Config, "eduni/namespaces/1.0", "017.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-018
%% Description:
%%   Simple legal case: default namespace
'rmt-ns10-018'(Config) -> run_test(Config, "eduni/namespaces/1.0", "018.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-019
%% Description:
%%   Simple legal case: prefixed element
'rmt-ns10-019'(Config) -> run_test(Config, "eduni/namespaces/1.0", "019.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-020
%% Description:
%%   Simple legal case: prefixed attribute
'rmt-ns10-020'(Config) -> run_test(Config, "eduni/namespaces/1.0", "020.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-021
%% Description:
%%   Simple legal case: default namespace and unbinding
'rmt-ns10-021'(Config) -> run_test(Config, "eduni/namespaces/1.0", "021.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-022
%% Description:
%%   Simple legal case: default namespace and rebinding
'rmt-ns10-022'(Config) -> run_test(Config, "eduni/namespaces/1.0", "022.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-023
%% Description:
%%   Illegal use of 1.1-style prefix unbinding in 1.0 document
'rmt-ns10-023'(Config) -> run_test(Config, "eduni/namespaces/1.0", "023.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-024
%% Description:
%%   Simple legal case: prefix rebinding
'rmt-ns10-024'(Config) -> run_test(Config, "eduni/namespaces/1.0", "024.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-025
%% Description:
%%   Unbound element prefix
'rmt-ns10-025'(Config) -> run_test(Config, "eduni/namespaces/1.0", "025.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-026
%% Description:
%%   Unbound attribute prefix
'rmt-ns10-026'(Config) -> run_test(Config, "eduni/namespaces/1.0", "026.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-027
%% Description:
%%   Reserved prefixes and namespaces: using the xml prefix undeclared
'rmt-ns10-027'(Config) -> run_test(Config, "eduni/namespaces/1.0", "027.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-028
%% Description:
%%   Reserved prefixes and namespaces: declaring the xml prefix correctly
'rmt-ns10-028'(Config) -> run_test(Config, "eduni/namespaces/1.0", "028.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-029
%% Description:
%%   Reserved prefixes and namespaces: declaring the xml prefix
%%   incorrectly
'rmt-ns10-029'(Config) -> run_test(Config, "eduni/namespaces/1.0", "029.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-030
%% Description:
%%   Reserved prefixes and namespaces: binding another prefix to the xml
%%   namespace
'rmt-ns10-030'(Config) -> run_test(Config, "eduni/namespaces/1.0", "030.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-031
%% Description:
%%   Reserved prefixes and namespaces: declaring the xmlns prefix with
%%   its correct URI (illegal)
'rmt-ns10-031'(Config) -> run_test(Config, "eduni/namespaces/1.0", "031.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-032
%% Description:
%%   Reserved prefixes and namespaces: declaring the xmlns prefix with an
%%   incorrect URI
'rmt-ns10-032'(Config) -> run_test(Config, "eduni/namespaces/1.0", "032.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-033
%% Description:
%%   Reserved prefixes and namespaces: binding another prefix to the
%%   xmlns namespace
'rmt-ns10-033'(Config) -> run_test(Config, "eduni/namespaces/1.0", "033.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-034
%% Description:
%%   Reserved prefixes and namespaces: binding a reserved prefix
'rmt-ns10-034'(Config) -> run_test(Config, "eduni/namespaces/1.0", "034.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-035
%% Description:
%%   Attribute uniqueness: repeated identical attribute
'rmt-ns10-035'(Config) -> run_test(Config, "eduni/namespaces/1.0", "035.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-036
%% Description:
%%   Attribute uniqueness: repeated attribute with different prefixes
'rmt-ns10-036'(Config) -> run_test(Config, "eduni/namespaces/1.0", "036.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-037
%% Description:
%%   Attribute uniqueness: different attributes with same local name
'rmt-ns10-037'(Config) -> run_test(Config, "eduni/namespaces/1.0", "037.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-038
%% Description:
%%   Attribute uniqueness: prefixed and unprefixed attributes with same
%%   local name
'rmt-ns10-038'(Config) -> run_test(Config, "eduni/namespaces/1.0", "038.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-039
%% Description:
%%   Attribute uniqueness: prefixed and unprefixed attributes with same
%%   local name, with default namespace
'rmt-ns10-039'(Config) -> run_test(Config, "eduni/namespaces/1.0", "039.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-040
%% Description:
%%   Attribute uniqueness: prefixed and unprefixed attributes with same
%%   local name, with default namespace and element in default namespace
'rmt-ns10-040'(Config) -> run_test(Config, "eduni/namespaces/1.0", "040.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-041
%% Description:
%%   Attribute uniqueness: prefixed and unprefixed attributes with same
%%   local name, element in same namespace as prefixed attribute
'rmt-ns10-041'(Config) -> run_test(Config, "eduni/namespaces/1.0", "041.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-042
%% Description:
%%   Colon in PI name
'rmt-ns10-042'(Config) -> run_test(Config, "eduni/namespaces/1.0", "042.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-043
%% Description:
%%   Colon in entity name
'rmt-ns10-043'(Config) -> run_test(Config, "eduni/namespaces/1.0", "043.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-044
%% Description:
%%   Colon in entity name
'rmt-ns10-044'(Config) -> run_test(Config, "eduni/namespaces/1.0", "044.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-045
%% Description:
%%   Colon in ID attribute name
'rmt-ns10-045'(Config) -> run_test(Config, "eduni/namespaces/1.0", "045.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-ns10-046
%% Description:
%%   Colon in ID attribute name
'rmt-ns10-046'(Config) -> run_test(Config, "eduni/namespaces/1.0", "046.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ht-ns10-047
%% Description:
%%   Reserved name: _not_ an error
'ht-ns10-047'(Config) -> run_test(Config, "eduni/namespaces/1.0", "047.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ht-ns10-048
%% Description:
%%   Reserved name: _not_ an error
'ht-ns10-048'(Config) -> run_test(Config, "eduni/namespaces/1.0", "048.xml", "valid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-ns11-001
%% Description:
%%   Namespace name test: a perfectly good http IRI that is not a URI
'rmt-ns11-001'(Config) -> run_test(Config, "eduni/namespaces/1.1", "001.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns11-002
%% Description:
%%   Namespace inequality test: different escaping of non-ascii letter
'rmt-ns11-002'(Config) -> run_test(Config, "eduni/namespaces/1.1", "002.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns11-003
%% Description:
%%   1.1 style prefix unbinding
'rmt-ns11-003'(Config) -> run_test(Config, "eduni/namespaces/1.1", "003.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns11-004
%% Description:
%%   1.1 style prefix unbinding and rebinding
'rmt-ns11-004'(Config) -> run_test(Config, "eduni/namespaces/1.1", "004.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-ns11-005
%% Description:
%%   Illegal use of prefix that has been unbound
'rmt-ns11-005'(Config) -> run_test(Config, "eduni/namespaces/1.1", "005.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns11-006
%% Description:
%%   Test whether non-Latin-1 characters are accepted in IRIs, and
%%   whether they are correctly distinguished
'rmt-ns11-006'(Config) -> run_test(Config, "eduni/namespaces/1.1", "006.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ht-bh-ns11-007
%% Description:
%%   Attempt to unbind xmlns 'namespace'
'ht-bh-ns11-007'(Config) -> run_test(Config, "eduni/namespaces/1.1", "007.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: ht-bh-ns11-008
%% Description:
%%   Attempt to unbind xml namespace
'ht-bh-ns11-008'(Config) -> run_test(Config, "eduni/namespaces/1.1", "008.xml", "not-wf").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-e3e-05a
%% Description:
%%   CDATA sections may occur in Mixed content.
'rmt-e3e-05a'(Config) -> run_test(Config, "eduni/errata-3e", "E05a.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-05b
%% Description:
%%   CDATA sections, comments and PIs may occur in ANY content.
'rmt-e3e-05b'(Config) -> run_test(Config, "eduni/errata-3e", "E05b.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06a
%% Description:
%%   Default values for IDREF attributes must match Name.
'rmt-e3e-06a'(Config) -> run_test(Config, "eduni/errata-3e", "E06a.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06b
%% Description:
%%   Default values for ENTITY attributes must match Name.
'rmt-e3e-06b'(Config) -> run_test(Config, "eduni/errata-3e", "E06b.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06c
%% Description:
%%   Default values for IDREFS attributes must match Names.
'rmt-e3e-06c'(Config) -> run_test(Config, "eduni/errata-3e", "E06c.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06d
%% Description:
%%   Default values for ENTITIES attributes must match Names.
'rmt-e3e-06d'(Config) -> run_test(Config, "eduni/errata-3e", "E06d.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06e
%% Description:
%%   Default values for NMTOKEN attributes must match Nmtoken.
'rmt-e3e-06e'(Config) -> run_test(Config, "eduni/errata-3e", "E06e.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06f
%% Description:
%%   Default values for NMTOKENS attributes must match Nmtokens.
'rmt-e3e-06f'(Config) -> run_test(Config, "eduni/errata-3e", "E06f.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06g
%% Description:
%%   Default values for NOTATION attributes must match one of the
%%   enumerated values.
'rmt-e3e-06g'(Config) -> run_test(Config, "eduni/errata-3e", "E06g.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06h
%% Description:
%%   Default values for enumerated attributes must match one of the
%%   enumerated values.
'rmt-e3e-06h'(Config) -> run_test(Config, "eduni/errata-3e", "E06h.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-06i
%% Description:
%%   Non-syntactic validity errors in default attributes only happen if
%%   the attribute is in fact defaulted.
'rmt-e3e-06i'(Config) -> run_test(Config, "eduni/errata-3e", "E06i.xml", "valid").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-12
%% Description:
%%   Default values for attributes may not contain references to external
%%   entities.
'rmt-e3e-12'(Config) -> run_test(Config, "eduni/errata-3e", "E12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-e3e-13
%% Description:
%%   Even internal parameter entity references are enough to make
%%   undeclared entities into mere validity errors rather than
%%   well-formedness errors.
'rmt-e3e-13'(Config) -> run_test(Config, "eduni/errata-3e", "E13.xml", "invalid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: invalid-bo-1
%% Entities: general
%% Output: inclbom_out.xml
%% Description:
%%   Byte order mark in general entity should go away (big-endian)
'invalid-bo-1'(Config) ->
    run_test(Config, "eduni/errata-4e", "inclbom_be.xml", "invalid", "inclbom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-2
%% Entities: general
%% Output: inclbom_out.xml
%% Description:
%%   Byte order mark in general entity should go away (little-endian)
'invalid-bo-2'(Config) ->
    run_test(Config, "eduni/errata-4e", "inclbom_le.xml", "invalid", "inclbom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-3
%% Entities: general
%% Output: inclbom_out.xml
%% Description:
%%   Byte order mark in general entity should go away (utf-8)
'invalid-bo-3'(Config) ->
    run_test(Config, "eduni/errata-4e", "incl8bom.xml", "invalid", "inclbom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-4
%% Entities: general
%% Output: inclbombom_out.xml
%% Description:
%%   Two byte order marks in general entity produce only one (big-endian)
'invalid-bo-4'(Config) ->
    run_test(Config, "eduni/errata-4e", "inclbombom_be.xml", "invalid", "inclbombom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-5
%% Entities: general
%% Output: inclbombom_out.xml
%% Description:
%%   Two byte order marks in general entity produce only one
%%   (little-endian)
'invalid-bo-5'(Config) ->
    run_test(Config, "eduni/errata-4e", "inclbombom_le.xml", "invalid", "inclbombom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-6
%% Entities: general
%% Output: inclbombom_out.xml
%% Description:
%%   Two byte order marks in general entity produce only one (utf-8)
'invalid-bo-6'(Config) ->
    run_test(Config, "eduni/errata-4e", "incl8bombom.xml", "invalid", "inclbombom_out.xml").

%%----------------------------------------------------------------------
%% ID: invalid-bo-7
%% Entities: general
%% Description:
%%   A byte order mark and a backwards one in general entity cause an
%%   illegal char. error (big-endian)
'invalid-bo-7'(Config) -> run_test(Config, "eduni/errata-4e", "inclbomboom_be.xml", "error").

%%----------------------------------------------------------------------
%% ID: invalid-bo-8
%% Entities: general
%% Description:
%%   A byte order mark and a backwards one in general entity cause an
%%   illegal char. error (little-endian)
'invalid-bo-8'(Config) -> run_test(Config, "eduni/errata-4e", "inclbomboom_le.xml", "error").

%%----------------------------------------------------------------------
%% ID: invalid-bo-9
%% Entities: general
%% Description:
%%   A byte order mark and a backwards one in general entity cause an
%%   illegal char. error (utf-8)
'invalid-bo-9'(Config) -> run_test(Config, "eduni/errata-4e", "incl8bomboom.xml", "error").

%%----------------------------------------------------------------------
%% ID: invalid-sa-140
%% Description:
%%   Character '&#x309a;' is a CombiningChar, not a Letter, but as of 5th
%%   edition, may begin a name (c.f. xmltest/not-wf/sa/140.xml).
'invalid-sa-140'(Config) -> run_test(Config, "eduni/errata-4e", "140.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: invalid-sa-141
%% Description:
%%   As of 5th edition, character #x0E5C is legal in XML names (c.f.
%%   xmltest/not-wf/sa/141.xml).
'invalid-sa-141'(Config) -> run_test(Config, "eduni/errata-4e", "141.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: x-rmt-008b
%% Description:
%%   a document with version=1.7, legal in XML 1.0 from 5th edition
'x-rmt-008b'(Config) -> run_test(Config, "eduni/errata-4e", "008.xml", "valid").

%%----------------------------------------------------------------------
%% ID: x-rmt5-014
%% Description:
%%   Has a "long s" in a name, legal in XML 1.1, legal in XML 1.0 5th
%%   edition
'x-rmt5-014'(Config) -> run_test(Config, "eduni/errata-4e", "014.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: x-rmt5-014a
%% Description:
%%   Has a "long s" in a name, legal in XML 1.1, legal in XML 1.0 5th
%%   edition
'x-rmt5-014a'(Config) -> run_test(Config, "eduni/errata-4e", "014a.xml", "valid").

%%----------------------------------------------------------------------
%% ID: x-rmt5-016
%% Description:
%%   Has a Byzantine Musical Symbol Kratimata in a name, legal in XML
%%   1.1, legal in XML 1.0 5th edition
'x-rmt5-016'(Config) -> run_test(Config, "eduni/errata-4e", "016.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: x-rmt5-019
%% Description:
%%   Has the last legal namechar in XML 1.1, legal in XML 1.0 5th edition
'x-rmt5-019'(Config) -> run_test(Config, "eduni/errata-4e", "019.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n02.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x333
'x-ibm-1-0.5-not-wf-P04-ibm04n02'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n03.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x369
'x-ibm-1-0.5-not-wf-P04-ibm04n03'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n04.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x37E
'x-ibm-1-0.5-not-wf-P04-ibm04n04'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n05.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2000
'x-ibm-1-0.5-not-wf-P04-ibm04n05'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n06.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2001
'x-ibm-1-0.5-not-wf-P04-ibm04n06'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n07.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2002
'x-ibm-1-0.5-not-wf-P04-ibm04n07'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n08.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2005
'x-ibm-1-0.5-not-wf-P04-ibm04n08'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n09.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200B
'x-ibm-1-0.5-not-wf-P04-ibm04n09'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n10.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200E
'x-ibm-1-0.5-not-wf-P04-ibm04n10'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n11.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x200F
'x-ibm-1-0.5-not-wf-P04-ibm04n11'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n12.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2069
'x-ibm-1-0.5-not-wf-P04-ibm04n12'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n13.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2190
'x-ibm-1-0.5-not-wf-P04-ibm04n13'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n14.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x23FF
'x-ibm-1-0.5-not-wf-P04-ibm04n14'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n15.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x280F
'x-ibm-1-0.5-not-wf-P04-ibm04n15'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n16.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2A00
'x-ibm-1-0.5-not-wf-P04-ibm04n16'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n17.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2EDC
'x-ibm-1-0.5-not-wf-P04-ibm04n17'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n18.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2B00
'x-ibm-1-0.5-not-wf-P04-ibm04n18'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n19.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x2BFF
'x-ibm-1-0.5-not-wf-P04-ibm04n19'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n20.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0x3000
'x-ibm-1-0.5-not-wf-P04-ibm04n20'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n21.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xD800
'x-ibm-1-0.5-not-wf-P04-ibm04n21'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n22.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xD801
'x-ibm-1-0.5-not-wf-P04-ibm04n22'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n23.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xDAFF
'x-ibm-1-0.5-not-wf-P04-ibm04n23'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n24.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xDFFF
'x-ibm-1-0.5-not-wf-P04-ibm04n24'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n25.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xEFFF
'x-ibm-1-0.5-not-wf-P04-ibm04n25'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n26.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xF1FF
'x-ibm-1-0.5-not-wf-P04-ibm04n26'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n27.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xF8FF
'x-ibm-1-0.5-not-wf-P04-ibm04n27'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04-ibm04n28.xml
%% Description:
%%   Tests an element with an illegal NameStartChar: #0xFFFFF
'x-ibm-1-0.5-not-wf-P04-ibm04n28'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04n28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an01.xml
%% Description:
%%   Tests an element with an illegal NameChar: #xB8
'x-ibm-1-0.5-not-wf-P04a-ibm04an01'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an02.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xA1
'x-ibm-1-0.5-not-wf-P04a-ibm04an02'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an03.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xAF
'x-ibm-1-0.5-not-wf-P04a-ibm04an03'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an04.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x37E
'x-ibm-1-0.5-not-wf-P04a-ibm04an04'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an05.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2000
'x-ibm-1-0.5-not-wf-P04a-ibm04an05'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an06.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2001
'x-ibm-1-0.5-not-wf-P04a-ibm04an06'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an07.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2002
'x-ibm-1-0.5-not-wf-P04a-ibm04an07'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an07.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an08.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2005
'x-ibm-1-0.5-not-wf-P04a-ibm04an08'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an08.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an09.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x200B
'x-ibm-1-0.5-not-wf-P04a-ibm04an09'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an09.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an10.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x200E
'x-ibm-1-0.5-not-wf-P04a-ibm04an10'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an10.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an11.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2038
'x-ibm-1-0.5-not-wf-P04a-ibm04an11'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an11.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an12.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2041
'x-ibm-1-0.5-not-wf-P04a-ibm04an12'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an12.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an13.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2190
'x-ibm-1-0.5-not-wf-P04a-ibm04an13'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an13.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an14.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x23FF
'x-ibm-1-0.5-not-wf-P04a-ibm04an14'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an14.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an15.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x280F
'x-ibm-1-0.5-not-wf-P04a-ibm04an15'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an15.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an16.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2A00
'x-ibm-1-0.5-not-wf-P04a-ibm04an16'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an16.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an17.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFDD0
'x-ibm-1-0.5-not-wf-P04a-ibm04an17'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an17.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an18.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFDEF
'x-ibm-1-0.5-not-wf-P04a-ibm04an18'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an18.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an19.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x2FFF
'x-ibm-1-0.5-not-wf-P04a-ibm04an19'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an19.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an20.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0x3000
'x-ibm-1-0.5-not-wf-P04a-ibm04an20'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an20.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an21.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xD800
'x-ibm-1-0.5-not-wf-P04a-ibm04an21'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an21.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an22.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xD801
'x-ibm-1-0.5-not-wf-P04a-ibm04an22'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an22.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an23.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xDAFF
'x-ibm-1-0.5-not-wf-P04a-ibm04an23'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an23.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an24.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xDFFF
'x-ibm-1-0.5-not-wf-P04a-ibm04an24'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an24.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an25.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xEFFF
'x-ibm-1-0.5-not-wf-P04a-ibm04an25'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an25.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an26.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xF1FF
'x-ibm-1-0.5-not-wf-P04a-ibm04an26'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an26.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an27.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xF8FF
'x-ibm-1-0.5-not-wf-P04a-ibm04an27'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an27.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P04a-ibm04an28.xml
%% Description:
%%   Tests an element with an illegal NameChar: #0xFFFFF
'x-ibm-1-0.5-not-wf-P04a-ibm04an28'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04an28.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n01.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x0B
'x-ibm-1-0.5-not-wf-P05-ibm05n01'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n01.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n02.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x300
'x-ibm-1-0.5-not-wf-P05-ibm05n02'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n02.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n03.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x36F
'x-ibm-1-0.5-not-wf-P05-ibm05n03'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n03.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n04.xml
%% Description:
%%   Tests an element with an illegal Name containing #0x203F
'x-ibm-1-0.5-not-wf-P05-ibm05n04'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n04.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n05.xml
%% Description:
%%   Tests an element with an illegal Name containing #x2040
'x-ibm-1-0.5-not-wf-P05-ibm05n05'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n05.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-not-wf-P05-ibm05n06.xml
%% Description:
%%   Tests an element with an illegal Name containing #0xB7
'x-ibm-1-0.5-not-wf-P05-ibm05n06'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05n06.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P04-ibm04v01.xml
%% Description:
%%   This test case covers legal NameStartChars character ranges plus
%%   discrete legal characters for production 04.

%% run_test(Config, "eduni/errata-4e", "ibm04v01.xml", "valid").
'x-ibm-1-0.5-valid-P04-ibm04v01'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P04-ibm04av01.xml
%% Description:
%%   This test case covers legal NameChars character ranges plus discrete
%%   legal characters for production 04a.
'x-ibm-1-0.5-valid-P04-ibm04av01'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm04av01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P05-ibm05v01.xml
%% Description:
%%   This test case covers legal Element Names as per production 5.

%% run_test(Config, "eduni/errata-4e", "ibm05v01.xml", "valid").
'x-ibm-1-0.5-valid-P05-ibm05v01'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P05-ibm05v02.xml
%% Description:
%%   This test case covers legal PITarget (Names) as per production 5.

%% run_test(Config, "eduni/errata-4e", "ibm05v02.xml", "valid").
'x-ibm-1-0.5-valid-P05-ibm05v02'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P05-ibm05v03.xml
%% Description:
%%   This test case covers legal Attribute (Names) as per production 5.

%% run_test(Config, "eduni/errata-4e", "ibm05v03.xml", "valid").
'x-ibm-1-0.5-valid-P05-ibm05v03'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P05-ibm05v04.xml
%% Description:
%%   This test case covers legal ID/IDREF (Names) as per production 5.
'x-ibm-1-0.5-valid-P05-ibm05v04'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm05v04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P05-ibm05v05.xml
%% Description:
%%   This test case covers legal ENTITY (Names) as per production 5.

%% run_test(Config, "eduni/errata-4e", "ibm05v05.xml", "valid").
'x-ibm-1-0.5-valid-P05-ibm05v05'(_Config) -> {skip, "Non-namespaced test"}.

%%----------------------------------------------------------------------
%% ID: x-ibm-1-0.5-valid-P047-ibm07v01.xml
%% Description:
%%   This test case covers legal NMTOKEN Name character ranges plus
%%   discrete legal characters for production 7.
'x-ibm-1-0.5-valid-P047-ibm07v01'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm07v01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n03.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0132 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n03'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n04.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0133 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n04'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n05.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x013F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n05'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n06.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0140 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n06'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n06.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n07.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0149 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n07'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n07.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n08.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x017F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n08'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n08.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n09.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01c4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n09'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n09.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n10.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01CC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n10'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n10.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n100.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0BB6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n100'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n100.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n101.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0BBA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n101'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n101.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n102.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C0D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n102'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n102.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n103.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C11 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n103'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n103.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n104.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C29 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n104'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n104.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n105.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C34 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n105'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n105.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n106.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C5F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n106'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n106.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n107.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C62 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n107'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n107.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n108.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C8D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n108'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n108.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n109.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0C91 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n109'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n109.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n11.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01F1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n11'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n11.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n110.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0CA9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n110'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n110.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n111.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0CB4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n111'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n111.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n112.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0CBA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n112'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n112.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n113.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0CDF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n113'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n113.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n114.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0CE2 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n114'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n114.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n115.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0D0D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n115'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n115.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n116.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0D11 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n116'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n116.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n117.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0D29 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n117'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n117.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n118.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0D3A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n118'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n118.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n119.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0D62 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n119'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n119.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n12.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01F3 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n12'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n12.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n120.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E2F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n120'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n120.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n121.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E31 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n121'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n121.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n122.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E34 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n122'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n122.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n123.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E46 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n123'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n123.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n124.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E83 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n124'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n124.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n125.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E85 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n125'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n125.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n126.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E89 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n126'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n126.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n127.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E8B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n127'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n127.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n128.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E8E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n128'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n128.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n129.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0E98 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n129'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n129.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n13.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01F6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n13'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n13.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n130.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EA0 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n130'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n130.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n131.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EA4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n131'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n131.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n132.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EA6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n132'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n132.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n133.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EA8 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n133'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n133.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n134.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EAC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n134'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n134.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n135.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EAF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n135'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n135.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n136.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EB1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n136'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n136.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n137.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EB4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n137'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n137.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n138.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EBE occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n138'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n138.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n139.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0EC5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n139'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n139.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n14.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01F9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n14'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n14.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n140.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0F48 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n140'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n140.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n141.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0F6A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n141'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n141.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n142.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x10C6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n142'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n142.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n143.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x10F7 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n143'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n143.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n144.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1011 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n144'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n144.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n145.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1104 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n145'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n145.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n146.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1108 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n146'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n146.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n147.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x110A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n147'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n147.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n148.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x110D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n148'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n148.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n149.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x113B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n149'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n149.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n15.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x01F9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n15'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n15.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n150.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x113F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n150'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n150.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n151.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1141 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n151'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n151.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n152.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x114D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n152'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n152.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n153.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x114f occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n153'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n153.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n154.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1151 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n154'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n154.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n155.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1156 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n155'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n155.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n156.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x115A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n156'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n156.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n157.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1162 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n157'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n157.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n158.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1164 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n158'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n158.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n159.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1166 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n159'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n159.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n16.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0230 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n16'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n160.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x116B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n160'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n160.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n161.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x116F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n161'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n161.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n162.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1174 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n162'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n162.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n163.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x119F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n163'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n163.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n164.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11AC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n164'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n164.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n165.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11B6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n165'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n165.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n166.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11B9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n166'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n166.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n167.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11BB occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n167'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n167.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n168.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11C3 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n168'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n168.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n169.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11F1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n169'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n169.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n17.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x02AF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n17'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n17.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n170.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x11FA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n170'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n170.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n171.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1E9C occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n171'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n171.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n172.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1EFA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n172'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n172.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n173.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F16 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n173'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n173.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n174.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F1E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n174'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n174.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n175.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F46 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n175'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n175.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n176.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F4F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n176'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n176.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n177.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F58 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n177'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n177.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n178.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F5A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n178'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n178.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n179.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F5C occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n179'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n179.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n18.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x02CF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n18'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n18.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n180.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F5E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n180'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n180.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n181.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1F7E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n181'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n181.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n182.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FB5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n182'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n182.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n183.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FBD occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n183'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n183.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n184.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FBF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n184'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n184.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n185.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FC5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n185'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n185.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n186.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FCD occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n186'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n186.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n187.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FD5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n187'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n187.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n188.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FDC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n188'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n188.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n189.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FED occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n189'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n189.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n19.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0387 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n19'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n19.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n190.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FF5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n190'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n190.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n191.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x1FFD occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n191'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n191.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n192.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x2127 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n192'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n192.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n193.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x212F occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n193'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n193.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n194.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x2183 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n194'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n194.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n195.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x3095 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n195'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n195.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n196.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x30FB occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n196'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n196.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n197.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x312D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n197'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n197.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n198.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #xD7A4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n198'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n198.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n20.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x038B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n20'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n20.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n21.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03A2 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n21'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n21.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n22.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03CF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n22'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n22.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n23.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03D7 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n23'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n23.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n24.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03DD occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n24'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n24.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n25.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03E1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n25'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n25.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n26.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x03F4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n26'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n26.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n27.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x040D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n27'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n27.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n28.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0450 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n28'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n28.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n29.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x045D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n29'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n29.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n30.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0482 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n30'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n30.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n31.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04C5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n31'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n31.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n32.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04C6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n32'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n32.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n33.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04C9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n33'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n33.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n34.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04EC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n34'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n34.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n35.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04ED occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n35'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n35.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n36.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04F6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n36'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n36.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n37.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x04FA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n37'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n37.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n38.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0557 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n38'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n38.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n39.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0558 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n39'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n39.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n40.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0587 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n40'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n40.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n41.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x05EB occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n41'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n41.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n42.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x05F3 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n42'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n42.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n43.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0620 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n43'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n43.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n44.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x063B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n44'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n44.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n45.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x064B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n45'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n45.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n46.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06B8 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n46'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n46.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n47.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06BF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n47'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n47.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n48.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06CF occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n48'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n48.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n49.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06D4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n49'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n49.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n50.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06D6 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n50'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n50.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n51.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x06E7 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n51'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n51.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n52.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x093A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n52'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n52.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n53.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x093E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n53'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n53.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n54.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0962 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n54'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n54.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n55.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x098D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n55'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n55.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n56.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0991 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n56'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n56.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n57.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0992 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n57'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n57.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n58.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09A9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n58'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n58.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n59.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09B1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n59'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n59.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n60.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09B5 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n60'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n60.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n61.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09BA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n61'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n61.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n62.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09DE occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n62'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n62.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n63.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09E2 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n63'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n63.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n64.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x09F2 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n64'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n64.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n65.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A0B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n65'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n65.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n66.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A11 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n66'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n66.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n67.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A29 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n67'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n67.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n68.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A31 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n68'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n68.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n69.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A34 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n69'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n69.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n70.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A37 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n70'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n70.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n71.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A3A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n71'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n71.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n72.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A5D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n72'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n72.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n73.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A70 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n73'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n73.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n74.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A75 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n74'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n74.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n75.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #xA84 occurs as the first character of the PITarget in the
%%   PI in the DTD.
'ibm-valid-P85-ibm85n75'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n75.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n76.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0ABC occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n76'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n76.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n77.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0A92 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n77'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n77.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n78.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0AA9 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n78'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n78.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n79.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0AB1 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n79'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n79.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n80.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0AB4 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n80'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n80.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n81.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0ABA occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n81'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n81.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n82.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B04 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n82'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n82.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n83.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B0D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n83'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n83.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n84.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B11 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n84'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n84.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n85.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B29 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n85'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n85.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n86.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B31 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n86'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n86.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n87.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B34 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n87'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n87.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n88.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B3A occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n88'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n88.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n89.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B3E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n89'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n89.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n90.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B5E occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n90'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n90.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n91.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B62 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n91'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n91.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n92.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B8B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n92'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n92.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n93.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B91 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n93'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n93.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n94.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B98 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n94'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n94.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n95.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B9B occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n95'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n95.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n96.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0B9D occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n96'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n96.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n97.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0BA0 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n97'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n97.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n98.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0BA7 occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n98'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n98.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P85-ibm85n99.xml
%% Description:
%%   Tests BaseChar with an only legal per 5th edition character. The
%%   character #x0BAB occurs as the first character of the PITarget in
%%   the PI in the DTD.
'ibm-valid-P85-ibm85n99'(Config) -> run_test(Config, "eduni/errata-4e", "ibm85n99.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P86-ibm86n01.xml
%% Description:
%%   Tests Ideographic with an only legal per 5th edition character. The
%%   character #x4CFF occurs as the first character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P86-ibm86n01'(Config) -> run_test(Config, "eduni/errata-4e", "ibm86n01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P86-ibm86n02.xml
%% Description:
%%   Tests Ideographic with an only legal per 5th edition character. The
%%   character #x9FA6 occurs as the first character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P86-ibm86n02'(Config) -> run_test(Config, "eduni/errata-4e", "ibm86n02.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P86-ibm86n03.xml
%% Description:
%%   Tests Ideographic with an only legal per 5th edition character. The
%%   character #x3008 occurs as the first character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P86-ibm86n03'(Config) -> run_test(Config, "eduni/errata-4e", "ibm86n03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P86-ibm86n04.xml
%% Description:
%%   Tests Ideographic with an only legal per 5th edition character. The
%%   character #x302A occurs as the first character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P86-ibm86n04'(Config) -> run_test(Config, "eduni/errata-4e", "ibm86n04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n01.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x02FF occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n01'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n01.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n02.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0346 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n02'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n02.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n03.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0362 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n03'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n04.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0487 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n04'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n05.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x05A2 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n05'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n06.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x05BA occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n06'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n06.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n07.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x05BE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n07'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n07.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n08.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x05C0 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n08'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n08.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n09.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x05C3 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n09'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n09.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n10.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0653 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n10'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n10.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n11.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x06B8 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n11'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n11.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n12.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x06B9 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n12'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n12.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n13.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x06E9 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n13'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n13.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n14.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x06EE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n14'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n14.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n15.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0904 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n15'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n15.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n16.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x093B occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n16'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n17.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x094E occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n17'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n17.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n18.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0955 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n18'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n18.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n19.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0964 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n19'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n19.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n20.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0984 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n20'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n20.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n21.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x09C5 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n21'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n21.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n22.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x09C9 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n22'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n22.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n23.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x09CE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n23'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n23.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n24.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x09D8 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n24'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n24.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n25.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x09E4 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n25'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n25.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n26.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A03 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n26'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n26.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n27.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A3D occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n27'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n27.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n28.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A46 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n28'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n28.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n29.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A49 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n29'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n29.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n30.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A4E occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n30'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n30.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n31.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A80 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n31'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n31.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n32.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0A84 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n32'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n32.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n33.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0ABB occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n33'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n33.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n34.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0AC6 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n34'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n34.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n35.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0ACA occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n35'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n35.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n36.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0ACE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n36'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n36.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n37.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B04 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n37'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n37.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n38.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B3B occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n38'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n38.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n39.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B44 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n39'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n39.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n40.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B4A occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n40'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n40.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n41.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B4E occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n41'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n41.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n42.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B58 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n42'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n42.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n43.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0B84 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n43'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n43.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n44.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0BC3 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n44'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n44.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n45.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0BC9 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n45'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n45.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n46.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0BD6 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n46'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n46.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n47.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C0D occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n47'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n47.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n48.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C45 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n48'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n48.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n49.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C49 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n49'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n49.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n50.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C54 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n50'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n50.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n51.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C81 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n51'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n51.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n52.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0C84 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n52'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n52.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n53.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0CC5 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n53'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n53.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n54.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0CC9 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n54'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n54.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n55.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0CD4 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n55'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n55.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n56.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0CD7 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n56'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n56.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n57.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0D04 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n57'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n57.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n58.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0D45 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n58'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n58.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n59.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0D49 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n59'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n59.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n60.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0D4E occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n60'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n60.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n61.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0D58 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n61'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n61.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n62.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0E3F occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n62'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n62.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n63.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0E3B occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n63'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n63.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n64.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0E4F occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n64'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n64.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n66.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0EBA occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n66'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n66.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n67.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0EBE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n67'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n67.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n68.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0ECE occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n68'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n68.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n69.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F1A occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n69'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n69.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n70.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F36 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n70'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n70.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n71.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F38 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n71'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n71.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n72.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F3B occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n72'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n72.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n73.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F3A occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n73'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n73.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n74.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F70 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n74'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n74.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n75.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F85 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n75'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n75.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n76.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F8C occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n76'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n76.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n77.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F96 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n77'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n77.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n78.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0F98 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n78'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n78.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n79.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0FB0 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n79'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n79.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n80.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0FB8 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n80'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n80.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n81.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x0FBA occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n81'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n81.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n82.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x20DD occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n82'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n82.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n83.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x20E2 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n83'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n83.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n84.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x3030 occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n84'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n84.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P87-ibm87n85.xml
%% Description:
%%   Tests CombiningChar with an only legal per 5th edition character.
%%   The character #x309B occurs as the second character in the PITarget
%%   in the PI in the DTD.
'ibm-valid-P87-ibm87n85'(Config) -> run_test(Config, "eduni/errata-4e", "ibm87n85.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n03.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x066A occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n03'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n04.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x06FA occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n04'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n05.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0970 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n05'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n06.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x09F2 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n06'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n06.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n08.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0AF0 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n08'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n08.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n09.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0B70 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n09'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n09.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n10.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0C65 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n10'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n10.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n11.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0CE5 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n11'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n11.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n12.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0CF0 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n12'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n12.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n13.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0D70 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n13'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n13.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n14.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0E5A occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n14'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n14.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n15.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0EDA occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n15'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n15.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P88-ibm88n16.xml
%% Description:
%%   Tests Digit with an only legal per 5th edition character. The
%%   character #x0F2A occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P88-ibm88n16'(Config) -> run_test(Config, "eduni/errata-4e", "ibm88n16.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P89-ibm89n03.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x02D2 occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P89-ibm89n03'(Config) -> run_test(Config, "eduni/errata-4e", "ibm89n03.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P89-ibm89n04.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x03FE occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P89-ibm89n04'(Config) -> run_test(Config, "eduni/errata-4e", "ibm89n04.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-valid-P89-ibm89n05.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x065F occurs as the second character in the PITarget in
%%   the PI in the DTD.
'ibm-valid-P89-ibm89n05'(Config) -> run_test(Config, "eduni/errata-4e", "ibm89n05.xml", "valid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n06.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x0EC7 occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n06'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n06.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n07.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x3006 occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n07'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n07.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n08.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x3030 occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n08'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n08.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n09.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x3036 occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n09'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n09.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n10.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x309C occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n10'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n10.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n11.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x309F occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n11'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n11.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: ibm-invalid-P89-ibm89n12.xml
%% Description:
%%   Tests Extender with an only legal per 5th edition character. The
%%   character #x30FF occurs as the second character in the PITarget in
%%   the PI in the prolog, and in an element name.
'ibm-invalid-P89-ibm89n12'(Config) ->
    run_test(Config, "eduni/errata-4e", "ibm89n12.xml", "invalid").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: rmt-ns-e1.0-13a
%% Description:
%%   The xml namespace must not be declared as the default namespace.
'rmt-ns-e1.0-13a'(Config) -> run_test(Config, "eduni/namespaces/errata-1e", "NE13a.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns-e1.0-13b
%% Description:
%%   The xmlns namespace must not be declared as the default namespace.
'rmt-ns-e1.0-13b'(Config) -> run_test(Config, "eduni/namespaces/errata-1e", "NE13b.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: rmt-ns-e1.0-13c
%% Description:
%%   Elements must not have the prefix xmlns.
'rmt-ns-e1.0-13c'(Config) -> run_test(Config, "eduni/namespaces/errata-1e", "NE13c.xml", "not-wf").

%%----------------------------------------------------------------------
%% Test Cases
%% Profile:
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% ID: hst-bh-001
%% Description:
%%   decimal charref > 10FFFF, indeed > max 32 bit integer, checking for
%%   recovery from possible overflow
'hst-bh-001'(Config) -> run_test(Config, "eduni/misc", "001.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: hst-bh-002
%% Description:
%%   hex charref > 10FFFF, indeed > max 32 bit integer, checking for
%%   recovery from possible overflow
'hst-bh-002'(Config) -> run_test(Config, "eduni/misc", "002.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: hst-bh-003
%% Description:
%%   decimal charref > 10FFFF, indeed > max 64 bit integer, checking for
%%   recovery from possible overflow
'hst-bh-003'(Config) -> run_test(Config, "eduni/misc", "003.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: hst-bh-004
%% Description:
%%   hex charref > 10FFFF, indeed > max 64 bit integer, checking for
%%   recovery from possible overflow
'hst-bh-004'(Config) -> run_test(Config, "eduni/misc", "004.xml", "not-wf").

%%----------------------------------------------------------------------
%% ID: hst-bh-005
%% Description:
%%   xmlns:xml is an attribute as far as validation is concerned and must
%%   be declared
'hst-bh-005'(Config) -> run_test(Config, "eduni/misc", "005.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: hst-bh-006
%% Description:
%%   xmlns:foo is an attribute as far as validation is concerned and must
%%   be declared
'hst-bh-006'(Config) -> run_test(Config, "eduni/misc", "006.xml", "invalid").

%%----------------------------------------------------------------------
%% ID: hst-lhs-007
%% Description:
%%   UTF-8 BOM plus xml decl of iso-8859-1 incompatible

%% run_test(Config, "eduni/misc", "007.xml", "not-wf").
'hst-lhs-007'(_Config) -> {skip, "Non-matched file encoding and declaration, app only sees UTF-8"}.

%%----------------------------------------------------------------------
%% ID: hst-lhs-008
%% Description:
%%   UTF-16 BOM plus xml decl of utf-8 (using UTF-16 coding) incompatible

%% run_test(Config, "eduni/misc", "008.xml", "not-wf").
'hst-lhs-008'(_Config) -> {skip, "Non-matched file encoding and declaration, app only sees UTF-8"}.

%%----------------------------------------------------------------------
%% ID: hst-lhs-009
%% Description:
%%   UTF-16 BOM plus xml decl of utf-8 (using UTF-8 coding) incompatible
'hst-lhs-009'(Config) -> run_test(Config, "eduni/misc", "009.xml", "not-wf").

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

% -define(dont_rm_test_dirs, true).
-ifndef(dont_rm_test_dirs).

init_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    ok = erl_tar:extract("eduni.tgz", [compressed]),
    ok = erl_tar:extract("ibm.tgz", [compressed]),
    ok = erl_tar:extract("japanese.tgz", [compressed]),
    ok = erl_tar:extract("oasis.tgz", [compressed]),
    ok = erl_tar:extract("sun.tgz", [compressed]),
    ok = erl_tar:extract("xmltest.tgz", [compressed]),
    ok = change_mode(["eduni", "ibm", "japanese", "oasis", "sun", "xmltest"]),
    [{timetrap, {seconds, 1}} | Config].

end_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    ok = rm_files(["eduni", "ibm", "japanese", "oasis", "sun", "xmltest"]),
    Config.

-else.

init_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    [{timetrap, {seconds, 1}} | Config].

end_per_suite(Config) -> Config.

-endif.

%% initialization before each testcase
init_per_testcase(_TestCase, Config) ->
    io:format("Config:\n~p\n", [Config]),
    {ok, _} = file:read_file_info(filename:join([privdir(Config)])),
    code:add_patha(privdir(Config)),
    [{timetrap, {seconds, 3}} | Config].

%% clean up after each testcase
end_per_testcase(_Func, _Config) -> ok.

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.

all() ->
    [
        {group, group_eduni_error_general_xml10_errata4e},
        %{group, group_eduni_error_general_xml11},
        {group, group_eduni_error_none_ns10},
        {group, group_eduni_error_none_xml10_errata2e},
        %{group, group_eduni_error_none_xml11},
        {group, group_eduni_invalid_general_xml10_errata4e},
        {group, group_eduni_invalid_none_},
        {group, group_eduni_invalid_none_ns10},
        {group, group_eduni_invalid_none_xml10_errata2e},
        {group, group_eduni_invalid_none_xml10_errata3e},
        {group, group_eduni_invalid_none_xml10_errata4e},
        %{group, group_eduni_invalid_none_xml11},
        {group, group_eduni_invalid_parameter_xml10_errata2e},
        {group, group_eduni_not_wf_general_xml10_errata2e},
        %{group, group_eduni_not_wf_general_xml11},
        {group, group_eduni_not_wf_none_},
        {group, group_eduni_not_wf_none_ns10},
        {group, group_eduni_not_wf_none_ns10_errata1e},
        %{group, group_eduni_not_wf_none_ns11},
        {group, group_eduni_not_wf_none_xml10_errata2e},
        {group, group_eduni_not_wf_none_xml10_errata3e},
        {group, group_eduni_not_wf_none_xml10_errata4e},
        %{group, group_eduni_not_wf_none_xml11},
        %{group, group_eduni_not_wf_parameter_xml11},
        {group, group_eduni_valid_both_xml10_errata2e},
        %{group, group_eduni_valid_general_xml11},
        {group, group_eduni_valid_none_ns10},
        %{group, group_eduni_valid_none_ns11},
        {group, group_eduni_valid_none_xml10_errata2e},
        {group, group_eduni_valid_none_xml10_errata3e},
        {group, group_eduni_valid_none_xml10_errata4e},
        %{group, group_eduni_valid_none_xml11},
        {group, group_eduni_valid_parameter_xml10_errata2e},
        {group, group_ibm_error_both_},
        {group, group_ibm_error_none_},
        {group, group_ibm_error_parameter_},
        {group, group_ibm_invalid_none_},
        %{group, group_ibm_invalid_none_xml11},
        {group, group_ibm_invalid_parameter_},
        %{group, group_ibm_not_wf_both_xml11},
        {group, group_ibm_not_wf_general_},
        %{group, group_ibm_not_wf_general_xml11},
        {group, group_ibm_not_wf_none_},
        %{group, group_ibm_not_wf_none_xml11},
        {group, group_ibm_not_wf_parameter_},
        {group, group_ibm_valid_both_},
        {group, group_ibm_valid_general_},
        %{group, group_ibm_valid_general_xml11},
        {group, group_ibm_valid_none_},
        %{group, group_ibm_valid_none_xml11},
        {group, group_ibm_valid_parameter_},
        {group, group_japanese_error_parameter_},
        {group, group_japanese_valid_parameter_},
        {group, group_oasis_error_none_},
        {group, group_oasis_invalid_none_},
        {group, group_oasis_not_wf_none_},
        {group, group_oasis_not_wf_parameter_},
        {group, group_oasis_valid_none_},
        {group, group_oasis_valid_parameter_},
        {group, group_sun_error_none_},
        {group, group_sun_invalid_none_},
        {group, group_sun_invalid_parameter_},
        {group, group_sun_not_wf_general_},
        {group, group_sun_not_wf_none_},
        {group, group_sun_not_wf_parameter_},
        {group, group_sun_valid_general_},
        {group, group_sun_valid_none_},
        {group, group_sun_valid_parameter_},
        {group, group_xmltest_error_both_},
        {group, group_xmltest_invalid_both_},
        {group, group_xmltest_not_wf_both_},
        {group, group_xmltest_not_wf_general_},
        {group, group_xmltest_not_wf_none_},
        {group, group_xmltest_not_wf_parameter_},
        {group, group_xmltest_valid_both_},
        {group, group_xmltest_valid_none_},
        {group, group_xmltest_valid_parameter_}
    ].

%% erlfmt-ignore
groups() ->
    [
      {group_eduni_error_general_xml10_errata4e, [parallel],
        [{group, testcases1}]},
      {testcases1, [parallel],
        ['invalid-bo-7', 'invalid-bo-8', 'invalid-bo-9']},
      {group_eduni_error_general_xml11, [parallel],
        [{group, testcases2}]},
      {testcases2, [parallel],
        ['rmt-009']},
      {group_eduni_error_none_ns10, [parallel],
        [{group, testcases3}]},
      {testcases3, [parallel],
        ['rmt-ns10-004', 'rmt-ns10-005', 'rmt-ns10-006']},
      {group_eduni_error_none_xml10_errata2e, [parallel],
        [{group, testcases4}]},
      {testcases4, [parallel],
        ['rmt-e2e-34', 'rmt-e2e-55', 'rmt-e2e-57']},
      {group_eduni_error_none_xml11, [parallel],
        [{group, testcases5}]},
      {testcases5, [parallel],
        ['rmt-008', 'rmt-055', 'rmt-056', 'rmt-057']},
      {group_eduni_invalid_general_xml10_errata4e, [parallel],
        [{group, testcases6}]},
      {testcases6, [parallel],
        ['invalid-bo-1', 'invalid-bo-2', 'invalid-bo-3', 'invalid-bo-4', 'invalid-bo-5', 'invalid-bo-6']},
      {group_eduni_invalid_none_, [parallel],
        [{group, testcases7}]},
      {testcases7, [parallel],
        ['hst-bh-005', 'hst-bh-006']},
      {group_eduni_invalid_none_ns10, [parallel],
        [{group, testcases8}]},
      {testcases8, [parallel],
        ['rmt-ns10-017', 'rmt-ns10-018', 'rmt-ns10-019', 'rmt-ns10-020', 'rmt-ns10-021', 'rmt-ns10-022',
         'rmt-ns10-024', 'rmt-ns10-027', 'rmt-ns10-028', 'rmt-ns10-034', 'rmt-ns10-037', 'rmt-ns10-038',
         'rmt-ns10-039', 'rmt-ns10-040', 'rmt-ns10-041', 'rmt-ns10-045', 'rmt-ns10-046']},
      {group_eduni_invalid_none_xml10_errata2e, [parallel],
        [{group, testcases9}]},
      {testcases9, [parallel],
        ['rmt-e2e-2a',  'rmt-e2e-2b',  'rmt-e2e-9b',  'rmt-e2e-15a', 'rmt-e2e-15b', 'rmt-e2e-15c', 'rmt-e2e-15d',
         'rmt-e2e-15g', 'rmt-e2e-15h', 'rmt-e2e-20']},
      {group_eduni_invalid_none_xml10_errata3e, [parallel],
        [{group, testcases10}]},
      {testcases10, [parallel],
        ['rmt-e3e-06a', 'rmt-e3e-06b', 'rmt-e3e-06c', 'rmt-e3e-06d', 'rmt-e3e-06e', 'rmt-e3e-06f', 'rmt-e3e-06g',
         'rmt-e3e-06h', 'rmt-e3e-13']},
      {group_eduni_invalid_none_xml10_errata4e, [parallel],
        [{group, testcases11}]},
      {testcases11, [parallel],
        ['invalid-sa-140',           'invalid-sa-141',           'x-rmt5-014',
         'x-rmt5-016',               'x-rmt5-019',               'ibm-invalid-P89-ibm89n06',
         'ibm-invalid-P89-ibm89n07', 'ibm-invalid-P89-ibm89n08', 'ibm-invalid-P89-ibm89n09',
         'ibm-invalid-P89-ibm89n10', 'ibm-invalid-P89-ibm89n11', 'ibm-invalid-P89-ibm89n12']},
      {group_eduni_invalid_none_xml11, [parallel],
        [{group, testcases12}]},
      {testcases12, [parallel],
        ['rmt-015', 'rmt-017', 'rmt-018', 'rmt-030', 'rmt-032', 'rmt-036', 'rmt-037', 'rmt-046', 'rmt-048', 'rmt-052',
         'rmt-053']},
      {group_eduni_invalid_parameter_xml10_errata2e, [parallel],
        [{group, testcases13}]},
      {testcases13, [parallel],
        ['rmt-e2e-14']},
      {group_eduni_not_wf_general_xml10_errata2e, [parallel],
        [{group, testcases14}]},
      {testcases14, [parallel],
        ['rmt-e2e-38']},
      {group_eduni_not_wf_general_xml11, [parallel],
        [{group, testcases15}]},
      {testcases15, [parallel],
        ['rmt-003', 'rmt-004', 'rmt-005']},
      {group_eduni_not_wf_none_, [parallel],
        [{group, testcases16}]},
      {testcases16, [parallel],
        ['hst-bh-001',  'hst-bh-002',  'hst-bh-003',  'hst-bh-004',  'hst-lhs-007', 'hst-lhs-008', 'hst-lhs-009']},
      {group_eduni_not_wf_none_ns10, [parallel],
        [{group, testcases17}]},
      {testcases17, [parallel],
        ['rmt-ns10-009', 'rmt-ns10-010', 'rmt-ns10-011', 'rmt-ns10-012', 'rmt-ns10-013', 'rmt-ns10-014',
         'rmt-ns10-015', 'rmt-ns10-016', 'rmt-ns10-023', 'rmt-ns10-025', 'rmt-ns10-026', 'rmt-ns10-029',
         'rmt-ns10-030', 'rmt-ns10-031', 'rmt-ns10-032', 'rmt-ns10-033', 'rmt-ns10-035', 'rmt-ns10-036',
         'rmt-ns10-042', 'rmt-ns10-043', 'rmt-ns10-044']},
      {group_eduni_not_wf_none_ns10_errata1e, [parallel],
        [{group, testcases18}]},
      {testcases18, [parallel],
        ['rmt-ns-e1.0-13a', 'rmt-ns-e1.0-13b', 'rmt-ns-e1.0-13c']},
      {group_eduni_not_wf_none_ns11, [parallel],
        [{group, testcases19}]},
      {testcases19, [parallel],
        ['rmt-ns11-005',   'ht-bh-ns11-007', 'ht-bh-ns11-008']},
      {group_eduni_not_wf_none_xml10_errata2e, [parallel],
        [{group, testcases20}]},
      {testcases20, [parallel],
        ['rmt-e2e-27', 'rmt-e2e-61']},
      {group_eduni_not_wf_none_xml10_errata3e, [parallel],
        [{group, testcases21}]},
      {testcases21, [parallel],
        ['rmt-e3e-12']},
      {group_eduni_not_wf_none_xml10_errata4e, [parallel],
        [{group, testcases22}]},
      {testcases22, [parallel],
        ['x-ibm-1-0.5-not-wf-P04-ibm04n02',   'x-ibm-1-0.5-not-wf-P04-ibm04n03',
         'x-ibm-1-0.5-not-wf-P04-ibm04n04',   'x-ibm-1-0.5-not-wf-P04-ibm04n05',
         'x-ibm-1-0.5-not-wf-P04-ibm04n06',   'x-ibm-1-0.5-not-wf-P04-ibm04n07',
         'x-ibm-1-0.5-not-wf-P04-ibm04n08',   'x-ibm-1-0.5-not-wf-P04-ibm04n09',
         'x-ibm-1-0.5-not-wf-P04-ibm04n10',   'x-ibm-1-0.5-not-wf-P04-ibm04n11',
         'x-ibm-1-0.5-not-wf-P04-ibm04n12',   'x-ibm-1-0.5-not-wf-P04-ibm04n13',
         'x-ibm-1-0.5-not-wf-P04-ibm04n14',   'x-ibm-1-0.5-not-wf-P04-ibm04n15',
         'x-ibm-1-0.5-not-wf-P04-ibm04n16',   'x-ibm-1-0.5-not-wf-P04-ibm04n17',
         'x-ibm-1-0.5-not-wf-P04-ibm04n18',   'x-ibm-1-0.5-not-wf-P04-ibm04n19',
         'x-ibm-1-0.5-not-wf-P04-ibm04n20',   'x-ibm-1-0.5-not-wf-P04-ibm04n21',
         'x-ibm-1-0.5-not-wf-P04-ibm04n22',   'x-ibm-1-0.5-not-wf-P04-ibm04n23',
         'x-ibm-1-0.5-not-wf-P04-ibm04n24',   'x-ibm-1-0.5-not-wf-P04-ibm04n25',
         'x-ibm-1-0.5-not-wf-P04-ibm04n26',   'x-ibm-1-0.5-not-wf-P04-ibm04n27',
         'x-ibm-1-0.5-not-wf-P04-ibm04n28',   'x-ibm-1-0.5-not-wf-P04a-ibm04an01',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an02', 'x-ibm-1-0.5-not-wf-P04a-ibm04an03',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an04', 'x-ibm-1-0.5-not-wf-P04a-ibm04an05',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an06', 'x-ibm-1-0.5-not-wf-P04a-ibm04an07',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an08', 'x-ibm-1-0.5-not-wf-P04a-ibm04an09',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an10', 'x-ibm-1-0.5-not-wf-P04a-ibm04an11',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an12', 'x-ibm-1-0.5-not-wf-P04a-ibm04an13',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an14', 'x-ibm-1-0.5-not-wf-P04a-ibm04an15',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an16', 'x-ibm-1-0.5-not-wf-P04a-ibm04an17',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an18', 'x-ibm-1-0.5-not-wf-P04a-ibm04an19',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an20', 'x-ibm-1-0.5-not-wf-P04a-ibm04an21',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an22', 'x-ibm-1-0.5-not-wf-P04a-ibm04an23',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an24', 'x-ibm-1-0.5-not-wf-P04a-ibm04an25',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an26', 'x-ibm-1-0.5-not-wf-P04a-ibm04an27',
         'x-ibm-1-0.5-not-wf-P04a-ibm04an28', 'x-ibm-1-0.5-not-wf-P05-ibm05n01',
         'x-ibm-1-0.5-not-wf-P05-ibm05n02',   'x-ibm-1-0.5-not-wf-P05-ibm05n03',
         'x-ibm-1-0.5-not-wf-P05-ibm05n04',   'x-ibm-1-0.5-not-wf-P05-ibm05n05',
         'x-ibm-1-0.5-not-wf-P05-ibm05n06']},
      {group_eduni_not_wf_none_xml11, [parallel],
        [{group, testcases23}]},
      {testcases23, [parallel],
        ['rmt-011', 'rmt-013', 'rmt-020', 'rmt-021', 'rmt-038', 'rmt-039', 'rmt-041', 'rmt-042']},
      {group_eduni_not_wf_parameter_xml11, [parallel],
        [{group, testcases24}]},
      {testcases24, [parallel],
        ['rmt-001', 'rmt-002']},
      {group_eduni_valid_both_xml10_errata2e, [parallel],
        [{group, testcases25}]},
      {testcases25, [parallel],
        ['rmt-e2e-18']},
      {group_eduni_valid_general_xml11, [parallel],
        [{group, testcases26}]},
      {testcases26, [parallel],
        ['rmt-006']},
      {group_eduni_valid_none_ns10, [parallel],
        [{group, testcases27}]},
      {testcases27, [parallel],
        ['rmt-ns10-001', 'rmt-ns10-002', 'rmt-ns10-003', 'rmt-ns10-007', 'rmt-ns10-008', 'ht-ns10-047',
         'ht-ns10-048']},
      {group_eduni_valid_none_ns11, [parallel],
        [{group, testcases28}]},
      {testcases28, [parallel],
        ['rmt-ns11-001', 'rmt-ns11-002', 'rmt-ns11-003', 'rmt-ns11-004', 'rmt-ns11-006']},
      {group_eduni_valid_none_xml10_errata2e, [parallel],
        [{group, testcases29}]},
      {testcases29, [parallel],
        ['rmt-e2e-9a',  'rmt-e2e-15e', 'rmt-e2e-15f', 'rmt-e2e-15i', 'rmt-e2e-15j', 'rmt-e2e-15k', 'rmt-e2e-15l',
         'rmt-e2e-22',  'rmt-e2e-24',  'rmt-e2e-29',  'rmt-e2e-41',  'rmt-e2e-48',  'rmt-e2e-50']},
      {group_eduni_valid_none_xml10_errata3e, [parallel],
        [{group, testcases30}]},
      {testcases30, [parallel],
        ['rmt-e3e-05a', 'rmt-e3e-05b', 'rmt-e3e-06i']},
      {group_eduni_valid_none_xml10_errata4e, [parallel],
        [{group, testcases31}]},
      {testcases31, [parallel],
        ['x-rmt-008b',                      'x-rmt5-014a',                     'x-ibm-1-0.5-valid-P04-ibm04v01',
         'x-ibm-1-0.5-valid-P04-ibm04av01', 'x-ibm-1-0.5-valid-P05-ibm05v01',  'x-ibm-1-0.5-valid-P05-ibm05v02',
         'x-ibm-1-0.5-valid-P05-ibm05v03',  'x-ibm-1-0.5-valid-P05-ibm05v04',  'x-ibm-1-0.5-valid-P05-ibm05v05',
         'x-ibm-1-0.5-valid-P047-ibm07v01', 'ibm-valid-P85-ibm85n03',          'ibm-valid-P85-ibm85n04',
         'ibm-valid-P85-ibm85n05',          'ibm-valid-P85-ibm85n06',          'ibm-valid-P85-ibm85n07',
         'ibm-valid-P85-ibm85n08',          'ibm-valid-P85-ibm85n09',          'ibm-valid-P85-ibm85n10',
         'ibm-valid-P85-ibm85n100',         'ibm-valid-P85-ibm85n101',         'ibm-valid-P85-ibm85n102',
         'ibm-valid-P85-ibm85n103',         'ibm-valid-P85-ibm85n104',         'ibm-valid-P85-ibm85n105',
         'ibm-valid-P85-ibm85n106',         'ibm-valid-P85-ibm85n107',         'ibm-valid-P85-ibm85n108',
         'ibm-valid-P85-ibm85n109',         'ibm-valid-P85-ibm85n11',          'ibm-valid-P85-ibm85n110',
         'ibm-valid-P85-ibm85n111',         'ibm-valid-P85-ibm85n112',         'ibm-valid-P85-ibm85n113',
         'ibm-valid-P85-ibm85n114',         'ibm-valid-P85-ibm85n115',         'ibm-valid-P85-ibm85n116',
         'ibm-valid-P85-ibm85n117',         'ibm-valid-P85-ibm85n118',         'ibm-valid-P85-ibm85n119',
         'ibm-valid-P85-ibm85n12',          'ibm-valid-P85-ibm85n120',         'ibm-valid-P85-ibm85n121',
         'ibm-valid-P85-ibm85n122',         'ibm-valid-P85-ibm85n123',         'ibm-valid-P85-ibm85n124',
         'ibm-valid-P85-ibm85n125',         'ibm-valid-P85-ibm85n126',         'ibm-valid-P85-ibm85n127',
         'ibm-valid-P85-ibm85n128',         'ibm-valid-P85-ibm85n129',         'ibm-valid-P85-ibm85n13',
         'ibm-valid-P85-ibm85n130',         'ibm-valid-P85-ibm85n131',         'ibm-valid-P85-ibm85n132',
         'ibm-valid-P85-ibm85n133',         'ibm-valid-P85-ibm85n134',         'ibm-valid-P85-ibm85n135',
         'ibm-valid-P85-ibm85n136',         'ibm-valid-P85-ibm85n137',         'ibm-valid-P85-ibm85n138',
         'ibm-valid-P85-ibm85n139',         'ibm-valid-P85-ibm85n14',          'ibm-valid-P85-ibm85n140',
         'ibm-valid-P85-ibm85n141',         'ibm-valid-P85-ibm85n142',         'ibm-valid-P85-ibm85n143',
         'ibm-valid-P85-ibm85n144',         'ibm-valid-P85-ibm85n145',         'ibm-valid-P85-ibm85n146',
         'ibm-valid-P85-ibm85n147',         'ibm-valid-P85-ibm85n148',         'ibm-valid-P85-ibm85n149',
         'ibm-valid-P85-ibm85n15',          'ibm-valid-P85-ibm85n150',         'ibm-valid-P85-ibm85n151',
         'ibm-valid-P85-ibm85n152',         'ibm-valid-P85-ibm85n153',         'ibm-valid-P85-ibm85n154',
         'ibm-valid-P85-ibm85n155',         'ibm-valid-P85-ibm85n156',         'ibm-valid-P85-ibm85n157',
         'ibm-valid-P85-ibm85n158',         'ibm-valid-P85-ibm85n159',         'ibm-valid-P85-ibm85n16',
         'ibm-valid-P85-ibm85n160',         'ibm-valid-P85-ibm85n161',         'ibm-valid-P85-ibm85n162',
         'ibm-valid-P85-ibm85n163',         'ibm-valid-P85-ibm85n164',         'ibm-valid-P85-ibm85n165',
         'ibm-valid-P85-ibm85n166',         'ibm-valid-P85-ibm85n167',         'ibm-valid-P85-ibm85n168',
         'ibm-valid-P85-ibm85n169',         'ibm-valid-P85-ibm85n17',          'ibm-valid-P85-ibm85n170',
         'ibm-valid-P85-ibm85n171',         'ibm-valid-P85-ibm85n172',         'ibm-valid-P85-ibm85n173',
         'ibm-valid-P85-ibm85n174',         'ibm-valid-P85-ibm85n175',         'ibm-valid-P85-ibm85n176',
         'ibm-valid-P85-ibm85n177',         'ibm-valid-P85-ibm85n178',         'ibm-valid-P85-ibm85n179',
         'ibm-valid-P85-ibm85n18',          'ibm-valid-P85-ibm85n180',         'ibm-valid-P85-ibm85n181',
         'ibm-valid-P85-ibm85n182',         'ibm-valid-P85-ibm85n183',         'ibm-valid-P85-ibm85n184',
         'ibm-valid-P85-ibm85n185',         'ibm-valid-P85-ibm85n186',         'ibm-valid-P85-ibm85n187',
         'ibm-valid-P85-ibm85n188',         'ibm-valid-P85-ibm85n189',         'ibm-valid-P85-ibm85n19',
         'ibm-valid-P85-ibm85n190',         'ibm-valid-P85-ibm85n191',         'ibm-valid-P85-ibm85n192',
         'ibm-valid-P85-ibm85n193',         'ibm-valid-P85-ibm85n194',         'ibm-valid-P85-ibm85n195',
         'ibm-valid-P85-ibm85n196',         'ibm-valid-P85-ibm85n197',         'ibm-valid-P85-ibm85n198',
         'ibm-valid-P85-ibm85n20',          'ibm-valid-P85-ibm85n21',          'ibm-valid-P85-ibm85n22',
         'ibm-valid-P85-ibm85n23',          'ibm-valid-P85-ibm85n24',          'ibm-valid-P85-ibm85n25',
         'ibm-valid-P85-ibm85n26',          'ibm-valid-P85-ibm85n27',          'ibm-valid-P85-ibm85n28',
         'ibm-valid-P85-ibm85n29',          'ibm-valid-P85-ibm85n30',          'ibm-valid-P85-ibm85n31',
         'ibm-valid-P85-ibm85n32',          'ibm-valid-P85-ibm85n33',          'ibm-valid-P85-ibm85n34',
         'ibm-valid-P85-ibm85n35',          'ibm-valid-P85-ibm85n36',          'ibm-valid-P85-ibm85n37',
         'ibm-valid-P85-ibm85n38',          'ibm-valid-P85-ibm85n39',          'ibm-valid-P85-ibm85n40',
         'ibm-valid-P85-ibm85n41',          'ibm-valid-P85-ibm85n42',          'ibm-valid-P85-ibm85n43',
         'ibm-valid-P85-ibm85n44',          'ibm-valid-P85-ibm85n45',          'ibm-valid-P85-ibm85n46',
         'ibm-valid-P85-ibm85n47',          'ibm-valid-P85-ibm85n48',          'ibm-valid-P85-ibm85n49',
         'ibm-valid-P85-ibm85n50',          'ibm-valid-P85-ibm85n51',          'ibm-valid-P85-ibm85n52',
         'ibm-valid-P85-ibm85n53',          'ibm-valid-P85-ibm85n54',          'ibm-valid-P85-ibm85n55',
         'ibm-valid-P85-ibm85n56',          'ibm-valid-P85-ibm85n57',          'ibm-valid-P85-ibm85n58',
         'ibm-valid-P85-ibm85n59',          'ibm-valid-P85-ibm85n60',          'ibm-valid-P85-ibm85n61',
         'ibm-valid-P85-ibm85n62',          'ibm-valid-P85-ibm85n63',          'ibm-valid-P85-ibm85n64',
         'ibm-valid-P85-ibm85n65',          'ibm-valid-P85-ibm85n66',          'ibm-valid-P85-ibm85n67',
         'ibm-valid-P85-ibm85n68',          'ibm-valid-P85-ibm85n69',          'ibm-valid-P85-ibm85n70',
         'ibm-valid-P85-ibm85n71',          'ibm-valid-P85-ibm85n72',          'ibm-valid-P85-ibm85n73',
         'ibm-valid-P85-ibm85n74',          'ibm-valid-P85-ibm85n75',          'ibm-valid-P85-ibm85n76',
         'ibm-valid-P85-ibm85n77',          'ibm-valid-P85-ibm85n78',          'ibm-valid-P85-ibm85n79',
         'ibm-valid-P85-ibm85n80',          'ibm-valid-P85-ibm85n81',          'ibm-valid-P85-ibm85n82',
         'ibm-valid-P85-ibm85n83',          'ibm-valid-P85-ibm85n84',          'ibm-valid-P85-ibm85n85',
         'ibm-valid-P85-ibm85n86',          'ibm-valid-P85-ibm85n87',          'ibm-valid-P85-ibm85n88',
         'ibm-valid-P85-ibm85n89',          'ibm-valid-P85-ibm85n90',          'ibm-valid-P85-ibm85n91',
         'ibm-valid-P85-ibm85n92',          'ibm-valid-P85-ibm85n93',          'ibm-valid-P85-ibm85n94',
         'ibm-valid-P85-ibm85n95',          'ibm-valid-P85-ibm85n96',          'ibm-valid-P85-ibm85n97',
         'ibm-valid-P85-ibm85n98',          'ibm-valid-P85-ibm85n99',          'ibm-valid-P86-ibm86n01',
         'ibm-valid-P86-ibm86n02',          'ibm-valid-P86-ibm86n03',          'ibm-valid-P86-ibm86n04',
         'ibm-valid-P87-ibm87n01',          'ibm-valid-P87-ibm87n02',          'ibm-valid-P87-ibm87n03',
         'ibm-valid-P87-ibm87n04',          'ibm-valid-P87-ibm87n05',          'ibm-valid-P87-ibm87n06',
         'ibm-valid-P87-ibm87n07',          'ibm-valid-P87-ibm87n08',          'ibm-valid-P87-ibm87n09',
         'ibm-valid-P87-ibm87n10',          'ibm-valid-P87-ibm87n11',          'ibm-valid-P87-ibm87n12',
         'ibm-valid-P87-ibm87n13',          'ibm-valid-P87-ibm87n14',          'ibm-valid-P87-ibm87n15',
         'ibm-valid-P87-ibm87n16',          'ibm-valid-P87-ibm87n17',          'ibm-valid-P87-ibm87n18',
         'ibm-valid-P87-ibm87n19',          'ibm-valid-P87-ibm87n20',          'ibm-valid-P87-ibm87n21',
         'ibm-valid-P87-ibm87n22',          'ibm-valid-P87-ibm87n23',          'ibm-valid-P87-ibm87n24',
         'ibm-valid-P87-ibm87n25',          'ibm-valid-P87-ibm87n26',          'ibm-valid-P87-ibm87n27',
         'ibm-valid-P87-ibm87n28',          'ibm-valid-P87-ibm87n29',          'ibm-valid-P87-ibm87n30',
         'ibm-valid-P87-ibm87n31',          'ibm-valid-P87-ibm87n32',          'ibm-valid-P87-ibm87n33',
         'ibm-valid-P87-ibm87n34',          'ibm-valid-P87-ibm87n35',          'ibm-valid-P87-ibm87n36',
         'ibm-valid-P87-ibm87n37',          'ibm-valid-P87-ibm87n38',          'ibm-valid-P87-ibm87n39',
         'ibm-valid-P87-ibm87n40',          'ibm-valid-P87-ibm87n41',          'ibm-valid-P87-ibm87n42',
         'ibm-valid-P87-ibm87n43',          'ibm-valid-P87-ibm87n44',          'ibm-valid-P87-ibm87n45',
         'ibm-valid-P87-ibm87n46',          'ibm-valid-P87-ibm87n47',          'ibm-valid-P87-ibm87n48',
         'ibm-valid-P87-ibm87n49',          'ibm-valid-P87-ibm87n50',          'ibm-valid-P87-ibm87n51',
         'ibm-valid-P87-ibm87n52',          'ibm-valid-P87-ibm87n53',          'ibm-valid-P87-ibm87n54',
         'ibm-valid-P87-ibm87n55',          'ibm-valid-P87-ibm87n56',          'ibm-valid-P87-ibm87n57',
         'ibm-valid-P87-ibm87n58',          'ibm-valid-P87-ibm87n59',          'ibm-valid-P87-ibm87n60',
         'ibm-valid-P87-ibm87n61',          'ibm-valid-P87-ibm87n62',          'ibm-valid-P87-ibm87n63',
         'ibm-valid-P87-ibm87n64',          'ibm-valid-P87-ibm87n66',          'ibm-valid-P87-ibm87n67',
         'ibm-valid-P87-ibm87n68',          'ibm-valid-P87-ibm87n69',          'ibm-valid-P87-ibm87n70',
         'ibm-valid-P87-ibm87n71',          'ibm-valid-P87-ibm87n72',          'ibm-valid-P87-ibm87n73',
         'ibm-valid-P87-ibm87n74',          'ibm-valid-P87-ibm87n75',          'ibm-valid-P87-ibm87n76',
         'ibm-valid-P87-ibm87n77',          'ibm-valid-P87-ibm87n78',          'ibm-valid-P87-ibm87n79',
         'ibm-valid-P87-ibm87n80',          'ibm-valid-P87-ibm87n81',          'ibm-valid-P87-ibm87n82',
         'ibm-valid-P87-ibm87n83',          'ibm-valid-P87-ibm87n84',          'ibm-valid-P87-ibm87n85',
         'ibm-valid-P88-ibm88n03',          'ibm-valid-P88-ibm88n04',          'ibm-valid-P88-ibm88n05',
         'ibm-valid-P88-ibm88n06',          'ibm-valid-P88-ibm88n08',          'ibm-valid-P88-ibm88n09',
         'ibm-valid-P88-ibm88n10',          'ibm-valid-P88-ibm88n11',          'ibm-valid-P88-ibm88n12',
         'ibm-valid-P88-ibm88n13',          'ibm-valid-P88-ibm88n14',          'ibm-valid-P88-ibm88n15',
         'ibm-valid-P88-ibm88n16',          'ibm-valid-P89-ibm89n03',          'ibm-valid-P89-ibm89n04',
         'ibm-valid-P89-ibm89n05']},
      {group_eduni_valid_none_xml11, [parallel],
        [{group, testcases32}]},
      {testcases32, [parallel],
        ['rmt-007', 'rmt-010', 'rmt-012', 'rmt-022', 'rmt-023', 'rmt-024', 'rmt-025', 'rmt-026', 'rmt-027', 'rmt-028',
         'rmt-029', 'rmt-031', 'rmt-033', 'rmt-034', 'rmt-035', 'rmt-040', 'rmt-043', 'rmt-044', 'rmt-045', 'rmt-047',
         'rmt-049', 'rmt-050', 'rmt-051', 'rmt-054']},
      {group_eduni_valid_parameter_xml10_errata2e, [parallel],
        [{group, testcases33}, {group, testcases34}]},
      {testcases33, [parallel],
        ['rmt-e2e-19']},
      {testcases34, [parallel],
        ['rmt-e2e-36', 'rmt-e2e-60']},
      {group_ibm_error_both_, [parallel],
        [{group, testcases35}, {group, testcases36}]},
      {testcases35, [parallel],
        ['ibm-invalid-P68-ibm68i03', 'ibm-invalid-P68-ibm68i04']},
      {testcases36, [parallel],
        ['ibm-invalid-P69-ibm69i03', 'ibm-invalid-P69-ibm69i04']},
      {group_ibm_error_none_, [parallel],
        [{group, testcases37}]},
      {testcases37, [parallel],
        ['ibm-not-wf-P69-ibm69n05']},
      {group_ibm_error_parameter_, [parallel],
        [{group, testcases38}, {group, testcases39}]},
      {testcases38, [parallel],
        ['ibm-invalid-P68-ibm68i01', 'ibm-invalid-P68-ibm68i02']},
      {testcases39, [parallel],
        ['ibm-invalid-P69-ibm69i01', 'ibm-invalid-P69-ibm69i02']},
      {group_ibm_invalid_none_, [parallel],
        [{group, testcases40}, {group, testcases41}, {group, testcases42}, {group, testcases43}, {group, testcases44},
         {group, testcases45}, {group, testcases46}, {group, testcases47}, {group, testcases48}, {group, testcases49}]},
      {testcases40, [parallel],
        ['ibm-invalid-P28-ibm28i01']},
      {testcases41, [parallel],
        ['ibm-invalid-P39-ibm39i01', 'ibm-invalid-P39-ibm39i02', 'ibm-invalid-P39-ibm39i03',
         'ibm-invalid-P39-ibm39i04']},
      {testcases42, [parallel],
        ['ibm-invalid-P41-ibm41i01', 'ibm-invalid-P41-ibm41i02']},
      {testcases43, [parallel],
        ['ibm-invalid-P45-ibm45i01']},
      {testcases44, [parallel],
        ['ibm-invalid-P51-ibm51i03']},
      {testcases45, [parallel],
        ['ibm-invalid-P56-ibm56i01', 'ibm-invalid-P56-ibm56i02', 'ibm-invalid-P56-ibm56i03',
         'ibm-invalid-P56-ibm56i05', 'ibm-invalid-P56-ibm56i06', 'ibm-invalid-P56-ibm56i07',
         'ibm-invalid-P56-ibm56i08', 'ibm-invalid-P56-ibm56i09', 'ibm-invalid-P56-ibm56i10',
         'ibm-invalid-P56-ibm56i11', 'ibm-invalid-P56-ibm56i12', 'ibm-invalid-P56-ibm56i13',
         'ibm-invalid-P56-ibm56i14', 'ibm-invalid-P56-ibm56i15', 'ibm-invalid-P56-ibm56i16',
         'ibm-invalid-P56-ibm56i17', 'ibm-invalid-P56-ibm56i18']},
      {testcases46, [parallel],
        ['ibm-invalid-P58-ibm58i01', 'ibm-invalid-P58-ibm58i02']},
      {testcases47, [parallel],
        ['ibm-invalid-P59-ibm59i01']},
      {testcases48, [parallel],
        ['ibm-invalid-P60-ibm60i01', 'ibm-invalid-P60-ibm60i02', 'ibm-invalid-P60-ibm60i03',
         'ibm-invalid-P60-ibm60i04']},
      {testcases49, [parallel],
        ['ibm-invalid-P76-ibm76i01']},
      {group_ibm_invalid_none_xml11, [parallel],
        [{group, testcases50}]},
      {testcases50, [parallel],
        ['ibm-1-1-valid-P46-ibm46i01', 'ibm-1-1-valid-P46-ibm46i02']},
      {group_ibm_invalid_parameter_, [parallel],
        [{group, testcases51}, {group, testcases52}, {group, testcases53}, {group, testcases54}]},
      {testcases51, [parallel],
        ['ibm-invalid-P32-ibm32i01', 'ibm-invalid-P32-ibm32i03', 'ibm-invalid-P32-ibm32i04']},
      {testcases52, [parallel],
        ['ibm-invalid-P49-ibm49i01']},
      {testcases53, [parallel],
        ['ibm-invalid-P50-ibm50i01']},
      {testcases54, [parallel],
        ['ibm-invalid-P51-ibm51i01']},
      {group_ibm_not_wf_both_xml11, [parallel],
        [{group, testcases55}]},
      {testcases55, [parallel],
        ['ibm-1-1-not-wf-P77-ibm77n21']},
      {group_ibm_not_wf_general_, [parallel],
        [{group, testcases56}, {group, testcases57}]},
      {testcases56, [parallel],
        ['ibm-not-wf-P77-ibm77n01', 'ibm-not-wf-P77-ibm77n02']},
      {testcases57, [parallel],
        ['ibm-not-wf-P78-ibm78n01', 'ibm-not-wf-P78-ibm78n02']},
      {group_ibm_not_wf_general_xml11, [parallel],
        [{group, testcases58}, {group, testcases59}]},
      {testcases58, [parallel],
        ['ibm-1-1-not-wf-P02-ibm02n64', 'ibm-1-1-not-wf-P02-ibm02n65', 'ibm-1-1-not-wf-P02-ibm02n66']},
      {testcases59, [parallel],
        ['ibm-1-1-not-wf-P77-ibm77n01', 'ibm-1-1-not-wf-P77-ibm77n02', 'ibm-1-1-not-wf-P77-ibm77n03',
         'ibm-1-1-not-wf-P77-ibm77n04', 'ibm-1-1-not-wf-P77-ibm77n05', 'ibm-1-1-not-wf-P77-ibm77n06',
         'ibm-1-1-not-wf-P77-ibm77n07', 'ibm-1-1-not-wf-P77-ibm77n08', 'ibm-1-1-not-wf-P77-ibm77n09',
         'ibm-1-1-not-wf-P77-ibm77n10', 'ibm-1-1-not-wf-P77-ibm77n11', 'ibm-1-1-not-wf-P77-ibm77n12',
         'ibm-1-1-not-wf-P77-ibm77n16', 'ibm-1-1-not-wf-P77-ibm77n17', 'ibm-1-1-not-wf-P77-ibm77n18',
         'ibm-1-1-not-wf-P77-ibm77n19', 'ibm-1-1-not-wf-P77-ibm77n20']},
      {group_ibm_not_wf_none_, [parallel],
        [{group, testcases60},  {group, testcases61},  {group, testcases62},  {group, testcases63},
         {group, testcases64},  {group, testcases65},  {group, testcases66},  {group, testcases67},
         {group, testcases68},  {group, testcases69},  {group, testcases70},  {group, testcases71},
         {group, testcases72},  {group, testcases73},  {group, testcases74},  {group, testcases75},
         {group, testcases76},  {group, testcases77},  {group, testcases78},  {group, testcases79},
         {group, testcases80},  {group, testcases81},  {group, testcases82},  {group, testcases83},
         {group, testcases84},  {group, testcases85},  {group, testcases86},  {group, testcases87},
         {group, testcases88},  {group, testcases89},  {group, testcases90},  {group, testcases91},
         {group, testcases92},  {group, testcases93},  {group, testcases94},  {group, testcases95},
         {group, testcases96},  {group, testcases97},  {group, testcases98},  {group, testcases99},
         {group, testcases100}, {group, testcases101}, {group, testcases102}, {group, testcases103},
         {group, testcases104}, {group, testcases105}, {group, testcases106}, {group, testcases107},
         {group, testcases108}, {group, testcases109}, {group, testcases110}, {group, testcases111},
         {group, testcases112}, {group, testcases113}, {group, testcases114}, {group, testcases115},
         {group, testcases116}, {group, testcases117}, {group, testcases118}, {group, testcases119},
         {group, testcases120}, {group, testcases121}, {group, testcases122}, {group, testcases123},
         {group, testcases124}]},
      {testcases60, [parallel],
        ['ibm-not-wf-P01-ibm01n01', 'ibm-not-wf-P01-ibm01n02', 'ibm-not-wf-P01-ibm01n03']},
      {testcases61, [parallel],
        ['ibm-not-wf-P02-ibm02n01', 'ibm-not-wf-P02-ibm02n02', 'ibm-not-wf-P02-ibm02n03', 'ibm-not-wf-P02-ibm02n04',
         'ibm-not-wf-P02-ibm02n05', 'ibm-not-wf-P02-ibm02n06', 'ibm-not-wf-P02-ibm02n07', 'ibm-not-wf-P02-ibm02n08',
         'ibm-not-wf-P02-ibm02n09', 'ibm-not-wf-P02-ibm02n10', 'ibm-not-wf-P02-ibm02n11', 'ibm-not-wf-P02-ibm02n12',
         'ibm-not-wf-P02-ibm02n13', 'ibm-not-wf-P02-ibm02n14', 'ibm-not-wf-P02-ibm02n15', 'ibm-not-wf-P02-ibm02n16',
         'ibm-not-wf-P02-ibm02n17', 'ibm-not-wf-P02-ibm02n18', 'ibm-not-wf-P02-ibm02n19', 'ibm-not-wf-P02-ibm02n20',
         'ibm-not-wf-P02-ibm02n21', 'ibm-not-wf-P02-ibm02n22', 'ibm-not-wf-P02-ibm02n23', 'ibm-not-wf-P02-ibm02n24',
         'ibm-not-wf-P02-ibm02n25', 'ibm-not-wf-P02-ibm02n26', 'ibm-not-wf-P02-ibm02n27', 'ibm-not-wf-P02-ibm02n28',
         'ibm-not-wf-P02-ibm02n29', 'ibm-not-wf-P02-ibm02n30', 'ibm-not-wf-P02-ibm02n31', 'ibm-not-wf-P02-ibm02n32',
         'ibm-not-wf-P02-ibm02n33']},
      {testcases62, [parallel],
        ['ibm-not-wf-P03-ibm03n01']},
      {testcases63, [parallel],
        ['ibm-not-wf-P04-ibm04n01', 'ibm-not-wf-P04-ibm04n02', 'ibm-not-wf-P04-ibm04n03', 'ibm-not-wf-P04-ibm04n04',
         'ibm-not-wf-P04-ibm04n05', 'ibm-not-wf-P04-ibm04n06', 'ibm-not-wf-P04-ibm04n07', 'ibm-not-wf-P04-ibm04n08',
         'ibm-not-wf-P04-ibm04n09', 'ibm-not-wf-P04-ibm04n10', 'ibm-not-wf-P04-ibm04n11', 'ibm-not-wf-P04-ibm04n12',
         'ibm-not-wf-P04-ibm04n13', 'ibm-not-wf-P04-ibm04n14', 'ibm-not-wf-P04-ibm04n15', 'ibm-not-wf-P04-ibm04n16',
         'ibm-not-wf-P04-ibm04n17', 'ibm-not-wf-P04-ibm04n18']},
      {testcases64, [parallel],
        ['ibm-not-wf-P05-ibm05n01', 'ibm-not-wf-P05-ibm05n02', 'ibm-not-wf-P05-ibm05n03']},
      {testcases65, [parallel],
        ['ibm-not-wf-P09-ibm09n01', 'ibm-not-wf-P09-ibm09n02', 'ibm-not-wf-P09-ibm09n03', 'ibm-not-wf-P09-ibm09n04']},
      {testcases66, [parallel],
        ['ibm-not-wf-P10-ibm10n01', 'ibm-not-wf-P10-ibm10n02', 'ibm-not-wf-P10-ibm10n03', 'ibm-not-wf-P10-ibm10n04',
         'ibm-not-wf-P10-ibm10n05', 'ibm-not-wf-P10-ibm10n06', 'ibm-not-wf-P10-ibm10n07', 'ibm-not-wf-P10-ibm10n08']},
      {testcases67, [parallel],
        ['ibm-not-wf-P11-ibm11n01', 'ibm-not-wf-P11-ibm11n02', 'ibm-not-wf-P11-ibm11n03', 'ibm-not-wf-P11-ibm11n04']},
      {testcases68, [parallel],
        ['ibm-not-wf-P12-ibm12n01', 'ibm-not-wf-P12-ibm12n02', 'ibm-not-wf-P12-ibm12n03']},
      {testcases69, [parallel],
        ['ibm-not-wf-P13-ibm13n01', 'ibm-not-wf-P13-ibm13n02', 'ibm-not-wf-P13-ibm13n03']},
      {testcases70, [parallel],
        ['ibm-not-wf-P14-ibm14n01', 'ibm-not-wf-P14-ibm14n02', 'ibm-not-wf-P14-ibm14n03']},
      {testcases71, [parallel],
        ['ibm-not-wf-P15-ibm15n01', 'ibm-not-wf-P15-ibm15n02', 'ibm-not-wf-P15-ibm15n03', 'ibm-not-wf-P15-ibm15n04']},
      {testcases72, [parallel],
        ['ibm-not-wf-P16-ibm16n01', 'ibm-not-wf-P16-ibm16n02', 'ibm-not-wf-P16-ibm16n03', 'ibm-not-wf-P16-ibm16n04']},
      {testcases73, [parallel],
        ['ibm-not-wf-P17-ibm17n01', 'ibm-not-wf-P17-ibm17n02', 'ibm-not-wf-P17-ibm17n03', 'ibm-not-wf-P17-ibm17n04']},
      {testcases74, [parallel],
        ['ibm-not-wf-P18-ibm18n01', 'ibm-not-wf-P18-ibm18n02']},
      {testcases75, [parallel],
        ['ibm-not-wf-P19-ibm19n01', 'ibm-not-wf-P19-ibm19n02', 'ibm-not-wf-P19-ibm19n03']},
      {testcases76, [parallel],
        ['ibm-not-wf-P20-ibm20n01']},
      {testcases77, [parallel],
        ['ibm-not-wf-P21-ibm21n01', 'ibm-not-wf-P21-ibm21n02', 'ibm-not-wf-P21-ibm21n03']},
      {testcases78, [parallel],
        ['ibm-not-wf-P22-ibm22n01', 'ibm-not-wf-P22-ibm22n02', 'ibm-not-wf-P22-ibm22n03']},
      {testcases79, [parallel],
        ['ibm-not-wf-P23-ibm23n01', 'ibm-not-wf-P23-ibm23n02', 'ibm-not-wf-P23-ibm23n03', 'ibm-not-wf-P23-ibm23n04',
         'ibm-not-wf-P23-ibm23n05', 'ibm-not-wf-P23-ibm23n06']},
      {testcases80, [parallel],
        ['ibm-not-wf-P24-ibm24n01', 'ibm-not-wf-P24-ibm24n02', 'ibm-not-wf-P24-ibm24n03', 'ibm-not-wf-P24-ibm24n04',
         'ibm-not-wf-P24-ibm24n05', 'ibm-not-wf-P24-ibm24n06', 'ibm-not-wf-P24-ibm24n07', 'ibm-not-wf-P24-ibm24n08',
         'ibm-not-wf-P24-ibm24n09']},
      {testcases81, [parallel],
        ['ibm-not-wf-P25-ibm25n01', 'ibm-not-wf-P25-ibm25n02']},
      {testcases82, [parallel],
        ['ibm-not-wf-P26-ibm26n01']},
      {testcases83, [parallel],
        ['ibm-not-wf-P27-ibm27n01']},
      {testcases84, [parallel],
        ['ibm-not-wf-P28-ibm28n01', 'ibm-not-wf-P28-ibm28n02', 'ibm-not-wf-P28-ibm28n03', 'ibm-not-wf-P28-ibm28n04',
         'ibm-not-wf-P28-ibm28n05', 'ibm-not-wf-P28-ibm28n06', 'ibm-not-wf-P28-ibm28n07', 'ibm-not-wf-P28-ibm28n08']},
      {testcases85, [parallel],
        ['ibm-not-wf-P29-ibm29n01', 'ibm-not-wf-P29-ibm29n02', 'ibm-not-wf-P29-ibm29n03', 'ibm-not-wf-P29-ibm29n04',
         'ibm-not-wf-P29-ibm29n05', 'ibm-not-wf-P29-ibm29n06', 'ibm-not-wf-P29-ibm29n07']},
      {testcases86, [parallel],
        ['ibm-not-wf-P32-ibm32n01', 'ibm-not-wf-P32-ibm32n02', 'ibm-not-wf-P32-ibm32n03', 'ibm-not-wf-P32-ibm32n04',
         'ibm-not-wf-P32-ibm32n05', 'ibm-not-wf-P32-ibm32n06', 'ibm-not-wf-P32-ibm32n07', 'ibm-not-wf-P32-ibm32n08']},
      {testcases87, [parallel],
        ['ibm-not-wf-P39-ibm39n01', 'ibm-not-wf-P39-ibm39n02', 'ibm-not-wf-P39-ibm39n03', 'ibm-not-wf-P39-ibm39n04',
         'ibm-not-wf-P39-ibm39n05', 'ibm-not-wf-P39-ibm39n06']},
      {testcases88, [parallel],
        ['ibm-not-wf-P40-ibm40n01', 'ibm-not-wf-P40-ibm40n02', 'ibm-not-wf-P40-ibm40n03', 'ibm-not-wf-P40-ibm40n04',
         'ibm-not-wf-P40-ibm40n05']},
      {testcases89, [parallel],
        ['ibm-not-wf-P41-ibm41n01', 'ibm-not-wf-P41-ibm41n02', 'ibm-not-wf-P41-ibm41n03', 'ibm-not-wf-P41-ibm41n04',
         'ibm-not-wf-P41-ibm41n05', 'ibm-not-wf-P41-ibm41n06', 'ibm-not-wf-P41-ibm41n07', 'ibm-not-wf-P41-ibm41n08',
         'ibm-not-wf-P41-ibm41n09', 'ibm-not-wf-P41-ibm41n10', 'ibm-not-wf-P41-ibm41n11', 'ibm-not-wf-P41-ibm41n12',
         'ibm-not-wf-P41-ibm41n13', 'ibm-not-wf-P41-ibm41n14']},
      {testcases90, [parallel],
        ['ibm-not-wf-P42-ibm42n01', 'ibm-not-wf-P42-ibm42n02', 'ibm-not-wf-P42-ibm42n03', 'ibm-not-wf-P42-ibm42n04',
         'ibm-not-wf-P42-ibm42n05']},
      {testcases91, [parallel],
        ['ibm-not-wf-P43-ibm43n01', 'ibm-not-wf-P43-ibm43n02', 'ibm-not-wf-P43-ibm43n04', 'ibm-not-wf-P43-ibm43n05']},
      {testcases92, [parallel],
        ['ibm-not-wf-P44-ibm44n01', 'ibm-not-wf-P44-ibm44n02', 'ibm-not-wf-P44-ibm44n03', 'ibm-not-wf-P44-ibm44n04']},
      {testcases93, [parallel],
        ['ibm-not-wf-P45-ibm45n01', 'ibm-not-wf-P45-ibm45n02', 'ibm-not-wf-P45-ibm45n03', 'ibm-not-wf-P45-ibm45n04',
         'ibm-not-wf-P45-ibm45n05', 'ibm-not-wf-P45-ibm45n06', 'ibm-not-wf-P45-ibm45n07', 'ibm-not-wf-P45-ibm45n08',
         'ibm-not-wf-P45-ibm45n09']},
      {testcases94, [parallel],
        ['ibm-not-wf-P46-ibm46n01', 'ibm-not-wf-P46-ibm46n02', 'ibm-not-wf-P46-ibm46n03', 'ibm-not-wf-P46-ibm46n04',
         'ibm-not-wf-P46-ibm46n05']},
      {testcases95, [parallel],
        ['ibm-not-wf-P47-ibm47n01', 'ibm-not-wf-P47-ibm47n02', 'ibm-not-wf-P47-ibm47n03', 'ibm-not-wf-P47-ibm47n04',
         'ibm-not-wf-P47-ibm47n05', 'ibm-not-wf-P47-ibm47n06']},
      {testcases96, [parallel],
        ['ibm-not-wf-P48-ibm48n01', 'ibm-not-wf-P48-ibm48n02', 'ibm-not-wf-P48-ibm48n03', 'ibm-not-wf-P48-ibm48n04',
         'ibm-not-wf-P48-ibm48n05', 'ibm-not-wf-P48-ibm48n06', 'ibm-not-wf-P48-ibm48n07']},
      {testcases97, [parallel],
        ['ibm-not-wf-P49-ibm49n01', 'ibm-not-wf-P49-ibm49n02', 'ibm-not-wf-P49-ibm49n03', 'ibm-not-wf-P49-ibm49n04',
         'ibm-not-wf-P49-ibm49n05', 'ibm-not-wf-P49-ibm49n06']},
      {testcases98, [parallel],
        ['ibm-not-wf-P50-ibm50n01', 'ibm-not-wf-P50-ibm50n02', 'ibm-not-wf-P50-ibm50n03', 'ibm-not-wf-P50-ibm50n04',
         'ibm-not-wf-P50-ibm50n05', 'ibm-not-wf-P50-ibm50n06', 'ibm-not-wf-P50-ibm50n07']},
      {testcases99, [parallel],
        ['ibm-not-wf-P51-ibm51n01', 'ibm-not-wf-P51-ibm51n02', 'ibm-not-wf-P51-ibm51n03', 'ibm-not-wf-P51-ibm51n04',
         'ibm-not-wf-P51-ibm51n05', 'ibm-not-wf-P51-ibm51n06', 'ibm-not-wf-P51-ibm51n07']},
      {testcases100, [parallel],
        ['ibm-not-wf-P52-ibm52n01', 'ibm-not-wf-P52-ibm52n02', 'ibm-not-wf-P52-ibm52n03', 'ibm-not-wf-P52-ibm52n04',
         'ibm-not-wf-P52-ibm52n05', 'ibm-not-wf-P52-ibm52n06']},
      {testcases101, [parallel],
        ['ibm-not-wf-P53-ibm53n01', 'ibm-not-wf-P53-ibm53n02', 'ibm-not-wf-P53-ibm53n03', 'ibm-not-wf-P53-ibm53n04',
         'ibm-not-wf-P53-ibm53n05', 'ibm-not-wf-P53-ibm53n06', 'ibm-not-wf-P53-ibm53n07', 'ibm-not-wf-P53-ibm53n08']},
      {testcases102, [parallel],
        ['ibm-not-wf-P54-ibm54n01', 'ibm-not-wf-P54-ibm54n02']},
      {testcases103, [parallel],
        ['ibm-not-wf-P55-ibm55n01', 'ibm-not-wf-P55-ibm55n02', 'ibm-not-wf-P55-ibm55n03']},
      {testcases104, [parallel],
        ['ibm-not-wf-P56-ibm56n01', 'ibm-not-wf-P56-ibm56n02', 'ibm-not-wf-P56-ibm56n03', 'ibm-not-wf-P56-ibm56n04',
         'ibm-not-wf-P56-ibm56n05', 'ibm-not-wf-P56-ibm56n06', 'ibm-not-wf-P56-ibm56n07']},
      {testcases105, [parallel],
        ['ibm-not-wf-P57-ibm57n01']},
      {testcases106, [parallel],
        ['ibm-not-wf-P58-ibm58n01', 'ibm-not-wf-P58-ibm58n02', 'ibm-not-wf-P58-ibm58n03', 'ibm-not-wf-P58-ibm58n04',
         'ibm-not-wf-P58-ibm58n05', 'ibm-not-wf-P58-ibm58n06', 'ibm-not-wf-P58-ibm58n07', 'ibm-not-wf-P58-ibm58n08']},
      {testcases107, [parallel],
        ['ibm-not-wf-P59-ibm59n01', 'ibm-not-wf-P59-ibm59n02', 'ibm-not-wf-P59-ibm59n03', 'ibm-not-wf-P59-ibm59n04',
         'ibm-not-wf-P59-ibm59n05', 'ibm-not-wf-P59-ibm59n06']},
      {testcases108, [parallel],
        ['ibm-not-wf-P60-ibm60n01', 'ibm-not-wf-P60-ibm60n02', 'ibm-not-wf-P60-ibm60n03', 'ibm-not-wf-P60-ibm60n04',
         'ibm-not-wf-P60-ibm60n05', 'ibm-not-wf-P60-ibm60n06', 'ibm-not-wf-P60-ibm60n07', 'ibm-not-wf-P60-ibm60n08']},
      {testcases109, [parallel],
        ['ibm-not-wf-P66-ibm66n01', 'ibm-not-wf-P66-ibm66n02', 'ibm-not-wf-P66-ibm66n03', 'ibm-not-wf-P66-ibm66n04',
         'ibm-not-wf-P66-ibm66n05', 'ibm-not-wf-P66-ibm66n06', 'ibm-not-wf-P66-ibm66n07', 'ibm-not-wf-P66-ibm66n08',
         'ibm-not-wf-P66-ibm66n09', 'ibm-not-wf-P66-ibm66n10', 'ibm-not-wf-P66-ibm66n11', 'ibm-not-wf-P66-ibm66n12',
         'ibm-not-wf-P66-ibm66n13', 'ibm-not-wf-P66-ibm66n14', 'ibm-not-wf-P66-ibm66n15']},
      {testcases110, [parallel],
        ['ibm-not-wf-P68-ibm68n01', 'ibm-not-wf-P68-ibm68n02', 'ibm-not-wf-P68-ibm68n03', 'ibm-not-wf-P68-ibm68n04',
         'ibm-not-wf-P68-ibm68n05', 'ibm-not-wf-P68-ibm68n07', 'ibm-not-wf-P68-ibm68n08', 'ibm-not-wf-P68-ibm68n09',
         'ibm-not-wf-P68-ibm68n10']},
      {testcases111, [parallel],
        ['ibm-not-wf-P69-ibm69n01', 'ibm-not-wf-P69-ibm69n02', 'ibm-not-wf-P69-ibm69n03', 'ibm-not-wf-P69-ibm69n04',
         'ibm-not-wf-P69-ibm69n06', 'ibm-not-wf-P69-ibm69n07']},
      {testcases112, [parallel],
        ['ibm-not-wf-P71-ibm70n01', 'ibm-not-wf-P71-ibm71n01', 'ibm-not-wf-P71-ibm71n02', 'ibm-not-wf-P71-ibm71n03',
         'ibm-not-wf-P71-ibm71n04', 'ibm-not-wf-P71-ibm71n05', 'ibm-not-wf-P71-ibm71n06', 'ibm-not-wf-P71-ibm71n07',
         'ibm-not-wf-P71-ibm71n08']},
      {testcases113, [parallel],
        ['ibm-not-wf-P72-ibm72n01', 'ibm-not-wf-P72-ibm72n02', 'ibm-not-wf-P72-ibm72n03', 'ibm-not-wf-P72-ibm72n04',
         'ibm-not-wf-P72-ibm72n05', 'ibm-not-wf-P72-ibm72n06', 'ibm-not-wf-P72-ibm72n07', 'ibm-not-wf-P72-ibm72n08',
         'ibm-not-wf-P72-ibm72n09']},
      {testcases114, [parallel],
        ['ibm-not-wf-P73-ibm73n01', 'ibm-not-wf-P73-ibm73n03']},
      {testcases115, [parallel],
        ['ibm-not-wf-P74-ibm74n01']},
      {testcases116, [parallel],
        ['ibm-not-wf-P75-ibm75n01', 'ibm-not-wf-P75-ibm75n02', 'ibm-not-wf-P75-ibm75n03', 'ibm-not-wf-P75-ibm75n04',
         'ibm-not-wf-P75-ibm75n05', 'ibm-not-wf-P75-ibm75n06', 'ibm-not-wf-P75-ibm75n07', 'ibm-not-wf-P75-ibm75n08',
         'ibm-not-wf-P75-ibm75n09', 'ibm-not-wf-P75-ibm75n10', 'ibm-not-wf-P75-ibm75n11', 'ibm-not-wf-P75-ibm75n12',
         'ibm-not-wf-P75-ibm75n13']},
      {testcases117, [parallel],
        ['ibm-not-wf-P76-ibm76n01', 'ibm-not-wf-P76-ibm76n02', 'ibm-not-wf-P76-ibm76n03', 'ibm-not-wf-P76-ibm76n04',
         'ibm-not-wf-P76-ibm76n05', 'ibm-not-wf-P76-ibm76n06', 'ibm-not-wf-P76-ibm76n07']},
      {testcases118, [parallel],
        ['ibm-not-wf-P80-ibm80n01', 'ibm-not-wf-P80-ibm80n02', 'ibm-not-wf-P80-ibm80n03', 'ibm-not-wf-P80-ibm80n04',
         'ibm-not-wf-P80-ibm80n05', 'ibm-not-wf-P80-ibm80n06']},
      {testcases119, [parallel],
        ['ibm-not-wf-P81-ibm81n01', 'ibm-not-wf-P81-ibm81n02', 'ibm-not-wf-P81-ibm81n03', 'ibm-not-wf-P81-ibm81n04',
         'ibm-not-wf-P81-ibm81n05', 'ibm-not-wf-P81-ibm81n06', 'ibm-not-wf-P81-ibm81n07', 'ibm-not-wf-P81-ibm81n08',
         'ibm-not-wf-P81-ibm81n09']},
      {testcases120, [parallel],
        ['ibm-not-wf-P82-ibm82n01', 'ibm-not-wf-P82-ibm82n02', 'ibm-not-wf-P82-ibm82n03', 'ibm-not-wf-P82-ibm82n04',
         'ibm-not-wf-P82-ibm82n05', 'ibm-not-wf-P82-ibm82n06', 'ibm-not-wf-P82-ibm82n07', 'ibm-not-wf-P82-ibm82n08']},
      {testcases121, [parallel],
        ['ibm-not-wf-P83-ibm83n01', 'ibm-not-wf-P83-ibm83n02', 'ibm-not-wf-P83-ibm83n03', 'ibm-not-wf-P83-ibm83n04',
         'ibm-not-wf-P83-ibm83n05', 'ibm-not-wf-P83-ibm83n06']},
      {testcases122, [parallel],
        ['ibm-not-wf-P85-ibm85n01', 'ibm-not-wf-P85-ibm85n02']},
      {testcases123, [parallel],
        ['ibm-not-wf-P88-ibm88n01', 'ibm-not-wf-P88-ibm88n02']},
      {testcases124, [parallel],
        ['ibm-not-wf-P89-ibm89n01', 'ibm-not-wf-P89-ibm89n02']},
      {group_ibm_not_wf_none_xml11, [parallel],
        [{group, testcases125}, {group, testcases126}, {group, testcases127}, {group, testcases128},
         {group, testcases129}]},
      {testcases125, [parallel],
        ['ibm-1-1-not-wf-P02-ibm02n01', 'ibm-1-1-not-wf-P02-ibm02n02', 'ibm-1-1-not-wf-P02-ibm02n03',
         'ibm-1-1-not-wf-P02-ibm02n04', 'ibm-1-1-not-wf-P02-ibm02n05', 'ibm-1-1-not-wf-P02-ibm02n06',
         'ibm-1-1-not-wf-P02-ibm02n07', 'ibm-1-1-not-wf-P02-ibm02n08', 'ibm-1-1-not-wf-P02-ibm02n09',
         'ibm-1-1-not-wf-P02-ibm02n10', 'ibm-1-1-not-wf-P02-ibm02n11', 'ibm-1-1-not-wf-P02-ibm02n12',
         'ibm-1-1-not-wf-P02-ibm02n14', 'ibm-1-1-not-wf-P02-ibm02n15', 'ibm-1-1-not-wf-P02-ibm02n16',
         'ibm-1-1-not-wf-P02-ibm02n17', 'ibm-1-1-not-wf-P02-ibm02n18', 'ibm-1-1-not-wf-P02-ibm02n19',
         'ibm-1-1-not-wf-P02-ibm02n20', 'ibm-1-1-not-wf-P02-ibm02n21', 'ibm-1-1-not-wf-P02-ibm02n22',
         'ibm-1-1-not-wf-P02-ibm02n23', 'ibm-1-1-not-wf-P02-ibm02n24', 'ibm-1-1-not-wf-P02-ibm02n25',
         'ibm-1-1-not-wf-P02-ibm02n26', 'ibm-1-1-not-wf-P02-ibm02n27', 'ibm-1-1-not-wf-P02-ibm02n28',
         'ibm-1-1-not-wf-P02-ibm02n29', 'ibm-1-1-not-wf-P02-ibm02n30', 'ibm-1-1-not-wf-P02-ibm02n31',
         'ibm-1-1-not-wf-P02-ibm02n32', 'ibm-1-1-not-wf-P02-ibm02n33', 'ibm-1-1-not-wf-P02-ibm02n34',
         'ibm-1-1-not-wf-P02-ibm02n35', 'ibm-1-1-not-wf-P02-ibm02n36', 'ibm-1-1-not-wf-P02-ibm02n37',
         'ibm-1-1-not-wf-P02-ibm02n38', 'ibm-1-1-not-wf-P02-ibm02n39', 'ibm-1-1-not-wf-P02-ibm02n40',
         'ibm-1-1-not-wf-P02-ibm02n41', 'ibm-1-1-not-wf-P02-ibm02n42', 'ibm-1-1-not-wf-P02-ibm02n43',
         'ibm-1-1-not-wf-P02-ibm02n44', 'ibm-1-1-not-wf-P02-ibm02n45', 'ibm-1-1-not-wf-P02-ibm02n46',
         'ibm-1-1-not-wf-P02-ibm02n47', 'ibm-1-1-not-wf-P02-ibm02n48', 'ibm-1-1-not-wf-P02-ibm02n49',
         'ibm-1-1-not-wf-P02-ibm02n50', 'ibm-1-1-not-wf-P02-ibm02n51', 'ibm-1-1-not-wf-P02-ibm02n52',
         'ibm-1-1-not-wf-P02-ibm02n53', 'ibm-1-1-not-wf-P02-ibm02n54', 'ibm-1-1-not-wf-P02-ibm02n55',
         'ibm-1-1-not-wf-P02-ibm02n56', 'ibm-1-1-not-wf-P02-ibm02n57', 'ibm-1-1-not-wf-P02-ibm02n58',
         'ibm-1-1-not-wf-P02-ibm02n59', 'ibm-1-1-not-wf-P02-ibm02n60', 'ibm-1-1-not-wf-P02-ibm02n61',
         'ibm-1-1-not-wf-P02-ibm02n62', 'ibm-1-1-not-wf-P02-ibm02n63', 'ibm-1-1-not-wf-P02-ibm02n67',
         'ibm-1-1-not-wf-P02-ibm02n68', 'ibm-1-1-not-wf-P02-ibm02n69', 'ibm-1-1-not-wf-P02-ibm02n70',
         'ibm-1-1-not-wf-P02-ibm02n71']},
      {testcases126, [parallel],
        ['ibm-1-1-not-wf-P04-ibm04n01', 'ibm-1-1-not-wf-P04-ibm04n02', 'ibm-1-1-not-wf-P04-ibm04n03',
         'ibm-1-1-not-wf-P04-ibm04n04', 'ibm-1-1-not-wf-P04-ibm04n05', 'ibm-1-1-not-wf-P04-ibm04n06',
         'ibm-1-1-not-wf-P04-ibm04n07', 'ibm-1-1-not-wf-P04-ibm04n08', 'ibm-1-1-not-wf-P04-ibm04n09',
         'ibm-1-1-not-wf-P04-ibm04n10', 'ibm-1-1-not-wf-P04-ibm04n11', 'ibm-1-1-not-wf-P04-ibm04n12',
         'ibm-1-1-not-wf-P04-ibm04n13', 'ibm-1-1-not-wf-P04-ibm04n14', 'ibm-1-1-not-wf-P04-ibm04n15',
         'ibm-1-1-not-wf-P04-ibm04n16', 'ibm-1-1-not-wf-P04-ibm04n17', 'ibm-1-1-not-wf-P04-ibm04n18',
         'ibm-1-1-not-wf-P04-ibm04n19', 'ibm-1-1-not-wf-P04-ibm04n20', 'ibm-1-1-not-wf-P04-ibm04n21',
         'ibm-1-1-not-wf-P04-ibm04n22', 'ibm-1-1-not-wf-P04-ibm04n23', 'ibm-1-1-not-wf-P04-ibm04n24',
         'ibm-1-1-not-wf-P04-ibm04n25', 'ibm-1-1-not-wf-P04-ibm04n26', 'ibm-1-1-not-wf-P04-ibm04n27',
         'ibm-1-1-not-wf-P04-ibm04n28']},
      {testcases127, [parallel],
        ['ibm-1-1-not-wf-P04a-ibm04an01', 'ibm-1-1-not-wf-P04a-ibm04an02', 'ibm-1-1-not-wf-P04a-ibm04an03',
         'ibm-1-1-not-wf-P04a-ibm04an04', 'ibm-1-1-not-wf-P04a-ibm04an05', 'ibm-1-1-not-wf-P04a-ibm04an06',
         'ibm-1-1-not-wf-P04a-ibm04an07', 'ibm-1-1-not-wf-P04a-ibm04an08', 'ibm-1-1-not-wf-P04a-ibm04an09',
         'ibm-1-1-not-wf-P04a-ibm04an10', 'ibm-1-1-not-wf-P04a-ibm04an11', 'ibm-1-1-not-wf-P04a-ibm04an12',
         'ibm-1-1-not-wf-P04a-ibm04an13', 'ibm-1-1-not-wf-P04a-ibm04an14', 'ibm-1-1-not-wf-P04a-ibm04an15',
         'ibm-1-1-not-wf-P04a-ibm04an16', 'ibm-1-1-not-wf-P04a-ibm04an17', 'ibm-1-1-not-wf-P04a-ibm04an18',
         'ibm-1-1-not-wf-P04a-ibm04an19', 'ibm-1-1-not-wf-P04a-ibm04an20', 'ibm-1-1-not-wf-P04a-ibm04an21',
         'ibm-1-1-not-wf-P04a-ibm04an22', 'ibm-1-1-not-wf-P04a-ibm04an23', 'ibm-1-1-not-wf-P04a-ibm04an24',
         'ibm-1-1-not-wf-P04a-ibm04an25', 'ibm-1-1-not-wf-P04a-ibm04an26', 'ibm-1-1-not-wf-P04a-ibm04an27',
         'ibm-1-1-not-wf-P04a-ibm04an28']},
      {testcases128, [parallel],
        ['ibm-1-1-not-wf-P05-ibm05n01', 'ibm-1-1-not-wf-P05-ibm05n02', 'ibm-1-1-not-wf-P05-ibm05n03',
         'ibm-1-1-not-wf-P05-ibm05n04', 'ibm-1-1-not-wf-P05-ibm05n05', 'ibm-1-1-not-wf-P05-ibm05n06']},
      {testcases129, [parallel],
        ['ibm-1-1-not-wf-P77-ibm77n13', 'ibm-1-1-not-wf-P77-ibm77n14', 'ibm-1-1-not-wf-P77-ibm77n15']},
      {group_ibm_not_wf_parameter_, [parallel],
        [{group, testcases130}, {group, testcases131}, {group, testcases132}, {group, testcases133},
         {group, testcases134}, {group, testcases135}, {group, testcases136}, {group, testcases137},
         {group, testcases138}, {group, testcases139}, {group, testcases140}, {group, testcases141}]},
      {testcases130, [parallel],
        ['ibm-not-wf-p28a-ibm28an01']},
      {testcases131, [parallel],
        ['ibm-not-wf-P30-ibm30n01']},
      {testcases132, [parallel],
        ['ibm-not-wf-P31-ibm31n01']},
      {testcases133, [parallel],
        ['ibm-not-wf-P32-ibm32n09']},
      {testcases134, [parallel],
        ['ibm-not-wf-P61-ibm61n01']},
      {testcases135, [parallel],
        ['ibm-not-wf-P62-ibm62n01', 'ibm-not-wf-P62-ibm62n02', 'ibm-not-wf-P62-ibm62n03', 'ibm-not-wf-P62-ibm62n04',
         'ibm-not-wf-P62-ibm62n05', 'ibm-not-wf-P62-ibm62n06', 'ibm-not-wf-P62-ibm62n07', 'ibm-not-wf-P62-ibm62n08']},
      {testcases136, [parallel],
        ['ibm-not-wf-P63-ibm63n01', 'ibm-not-wf-P63-ibm63n02', 'ibm-not-wf-P63-ibm63n03', 'ibm-not-wf-P63-ibm63n04',
         'ibm-not-wf-P63-ibm63n05', 'ibm-not-wf-P63-ibm63n06', 'ibm-not-wf-P63-ibm63n07']},
      {testcases137, [parallel],
        ['ibm-not-wf-P64-ibm64n01', 'ibm-not-wf-P64-ibm64n02', 'ibm-not-wf-P64-ibm64n03']},
      {testcases138, [parallel],
        ['ibm-not-wf-P65-ibm65n01', 'ibm-not-wf-P65-ibm65n02']},
      {testcases139, [parallel],
        ['ibm-not-wf-P68-ibm68n06']},
      {testcases140, [parallel],
        ['ibm-not-wf-P77-ibm77n03', 'ibm-not-wf-P77-ibm77n04']},
      {testcases141, [parallel],
        ['ibm-not-wf-P79-ibm79n01', 'ibm-not-wf-P79-ibm79n02']},
      {group_ibm_valid_both_, [parallel],
        [{group, testcases142}, {group, testcases143}]},
      {testcases142, [parallel],
        ['ibm-valid-P68-ibm68v02']},
      {testcases143, [parallel],
        ['ibm-valid-P69-ibm69v02']},
      {group_ibm_valid_general_, [parallel],
        [{group, testcases144}]},
      {testcases144, [parallel],
        ['ibm-valid-P78-ibm78v01']},
      {group_ibm_valid_general_xml11, [parallel],
        [{group, testcases145}, {group, testcases146}, {group, testcases147}]},
      {testcases145, [parallel],
        ['ibm-1-1-valid-P02-ibm02v06']},
      {testcases146, [parallel],
        ['ibm-1-1-valid-P03-ibm03v01', 'ibm-1-1-valid-P03-ibm03v02', 'ibm-1-1-valid-P03-ibm03v03',
         'ibm-1-1-valid-P03-ibm03v04', 'ibm-1-1-valid-P03-ibm03v09']},
      {testcases147, [parallel],
        ['ibm-1-1-valid-P77-ibm77v01', 'ibm-1-1-valid-P77-ibm77v02', 'ibm-1-1-valid-P77-ibm77v03',
         'ibm-1-1-valid-P77-ibm77v04', 'ibm-1-1-valid-P77-ibm77v05', 'ibm-1-1-valid-P77-ibm77v06',
         'ibm-1-1-valid-P77-ibm77v10', 'ibm-1-1-valid-P77-ibm77v11', 'ibm-1-1-valid-P77-ibm77v12',
         'ibm-1-1-valid-P77-ibm77v16', 'ibm-1-1-valid-P77-ibm77v17', 'ibm-1-1-valid-P77-ibm77v18',
         'ibm-1-1-valid-P77-ibm77v19', 'ibm-1-1-valid-P77-ibm77v20', 'ibm-1-1-valid-P77-ibm77v21',
         'ibm-1-1-valid-P77-ibm77v22', 'ibm-1-1-valid-P77-ibm77v23', 'ibm-1-1-valid-P77-ibm77v24',
         'ibm-1-1-valid-P77-ibm77v25', 'ibm-1-1-valid-P77-ibm77v26', 'ibm-1-1-valid-P77-ibm77v27',
         'ibm-1-1-valid-P77-ibm77v28', 'ibm-1-1-valid-P77-ibm77v29', 'ibm-1-1-valid-P77-ibm77v30']},
      {group_ibm_valid_none_, [parallel],
        [{group, testcases148}, {group, testcases149}, {group, testcases150}, {group, testcases151},
         {group, testcases152}, {group, testcases153}, {group, testcases154}, {group, testcases155},
         {group, testcases156}, {group, testcases157}, {group, testcases158}, {group, testcases159},
         {group, testcases160}, {group, testcases161}, {group, testcases162}, {group, testcases163},
         {group, testcases164}, {group, testcases165}, {group, testcases166}, {group, testcases167},
         {group, testcases168}, {group, testcases169}, {group, testcases170}, {group, testcases171},
         {group, testcases172}, {group, testcases173}, {group, testcases174}, {group, testcases175},
         {group, testcases176}, {group, testcases177}, {group, testcases178}, {group, testcases179},
         {group, testcases180}, {group, testcases181}, {group, testcases182}, {group, testcases183},
         {group, testcases184}, {group, testcases185}, {group, testcases186}, {group, testcases187},
         {group, testcases188}, {group, testcases189}, {group, testcases190}, {group, testcases191},
         {group, testcases192}, {group, testcases193}, {group, testcases194}, {group, testcases195},
         {group, testcases196}, {group, testcases197}, {group, testcases198}, {group, testcases199},
         {group, testcases200}, {group, testcases201}]},
      {testcases148, [parallel],
        ['ibm-valid-P01-ibm01v01']},
      {testcases149, [parallel],
        ['ibm-valid-P02-ibm02v01']},
      {testcases150, [parallel],
        ['ibm-valid-P03-ibm03v01']},
      {testcases151, [parallel],
        ['ibm-valid-P09-ibm09v01', 'ibm-valid-P09-ibm09v02', 'ibm-valid-P09-ibm09v04']},
      {testcases152, [parallel],
        ['ibm-valid-P10-ibm10v01', 'ibm-valid-P10-ibm10v02', 'ibm-valid-P10-ibm10v03', 'ibm-valid-P10-ibm10v04',
         'ibm-valid-P10-ibm10v05', 'ibm-valid-P10-ibm10v06', 'ibm-valid-P10-ibm10v07', 'ibm-valid-P10-ibm10v08']},
      {testcases153, [parallel],
        ['ibm-valid-P11-ibm11v01', 'ibm-valid-P11-ibm11v02']},
      {testcases154, [parallel],
        ['ibm-valid-P14-ibm14v01', 'ibm-valid-P14-ibm14v02', 'ibm-valid-P14-ibm14v03']},
      {testcases155, [parallel],
        ['ibm-valid-P15-ibm15v01', 'ibm-valid-P15-ibm15v02', 'ibm-valid-P15-ibm15v03', 'ibm-valid-P15-ibm15v04']},
      {testcases156, [parallel],
        ['ibm-valid-P16-ibm16v01', 'ibm-valid-P16-ibm16v02', 'ibm-valid-P16-ibm16v03']},
      {testcases157, [parallel],
        ['ibm-valid-P17-ibm17v01']},
      {testcases158, [parallel],
        ['ibm-valid-P18-ibm18v01']},
      {testcases159, [parallel],
        ['ibm-valid-P19-ibm19v01']},
      {testcases160, [parallel],
        ['ibm-valid-P20-ibm20v01', 'ibm-valid-P20-ibm20v02']},
      {testcases161, [parallel],
        ['ibm-valid-P21-ibm21v01']},
      {testcases162, [parallel],
        ['ibm-valid-P22-ibm22v01', 'ibm-valid-P22-ibm22v02', 'ibm-valid-P22-ibm22v03', 'ibm-valid-P22-ibm22v04',
         'ibm-valid-P22-ibm22v05', 'ibm-valid-P22-ibm22v06', 'ibm-valid-P22-ibm22v07']},
      {testcases163, [parallel],
        ['ibm-valid-P23-ibm23v01', 'ibm-valid-P23-ibm23v02', 'ibm-valid-P23-ibm23v03', 'ibm-valid-P23-ibm23v04',
         'ibm-valid-P23-ibm23v05', 'ibm-valid-P23-ibm23v06']},
      {testcases164, [parallel],
        ['ibm-valid-P24-ibm24v01', 'ibm-valid-P24-ibm24v02']},
      {testcases165, [parallel],
        ['ibm-valid-P25-ibm25v01', 'ibm-valid-P25-ibm25v02', 'ibm-valid-P25-ibm25v03', 'ibm-valid-P25-ibm25v04']},
      {testcases166, [parallel],
        ['ibm-valid-P26-ibm26v01']},
      {testcases167, [parallel],
        ['ibm-valid-P27-ibm27v01', 'ibm-valid-P27-ibm27v02', 'ibm-valid-P27-ibm27v03']},
      {testcases168, [parallel],
        ['ibm-valid-P28-ibm28v01']},
      {testcases169, [parallel],
        ['ibm-valid-P29-ibm29v01']},
      {testcases170, [parallel],
        ['ibm-valid-P33-ibm33v01']},
      {testcases171, [parallel],
        ['ibm-valid-P34-ibm34v01']},
      {testcases172, [parallel],
        ['ibm-valid-P35-ibm35v01']},
      {testcases173, [parallel],
        ['ibm-valid-P36-ibm36v01']},
      {testcases174, [parallel],
        ['ibm-valid-P37-ibm37v01']},
      {testcases175, [parallel],
        ['ibm-valid-P38-ibm38v01']},
      {testcases176, [parallel],
        ['ibm-valid-P39-ibm39v01']},
      {testcases177, [parallel],
        ['ibm-valid-P40-ibm40v01']},
      {testcases178, [parallel],
        ['ibm-valid-P41-ibm41v01']},
      {testcases179, [parallel],
        ['ibm-valid-P42-ibm42v01']},
      {testcases180, [parallel],
        ['ibm-valid-P43-ibm43v01']},
      {testcases181, [parallel],
        ['ibm-valid-P44-ibm44v01']},
      {testcases182, [parallel],
        ['ibm-valid-P45-ibm45v01']},
      {testcases183, [parallel],
        ['ibm-valid-P47-ibm47v01']},
      {testcases184, [parallel],
        ['ibm-valid-P51-ibm51v01']},
      {testcases185, [parallel],
        ['ibm-valid-P52-ibm52v01']},
      {testcases186, [parallel],
        ['ibm-valid-P54-ibm54v01']},
      {testcases187, [parallel],
        ['ibm-valid-P54-ibm54v02', 'ibm-valid-P54-ibm54v03']},
      {testcases188, [parallel],
        ['ibm-valid-P55-ibm55v01']},
      {testcases189, [parallel],
        ['ibm-valid-P56-ibm56v01', 'ibm-valid-P56-ibm56v02', 'ibm-valid-P56-ibm56v03', 'ibm-valid-P56-ibm56v04',
         'ibm-valid-P56-ibm56v05', 'ibm-valid-P56-ibm56v06', 'ibm-valid-P56-ibm56v07', 'ibm-valid-P56-ibm56v08',
         'ibm-valid-P56-ibm56v09', 'ibm-valid-P56-ibm56v10']},
      {testcases190, [parallel],
        ['ibm-valid-P57-ibm57v01']},
      {testcases191, [parallel],
        ['ibm-valid-P58-ibm58v01', 'ibm-valid-P58-ibm58v02']},
      {testcases192, [parallel],
        ['ibm-valid-P59-ibm59v01', 'ibm-valid-P59-ibm59v02']},
      {testcases193, [parallel],
        ['ibm-valid-P60-ibm60v01', 'ibm-valid-P60-ibm60v02', 'ibm-valid-P60-ibm60v03', 'ibm-valid-P60-ibm60v04']},
      {testcases194, [parallel],
        ['ibm-valid-P66-ibm66v01']},
      {testcases195, [parallel],
        ['ibm-valid-P67-ibm67v01']},
      {testcases196, [parallel],
        ['ibm-valid-P82-ibm82v01']},
      {testcases197, [parallel],
        ['ibm-valid-P85-ibm85v01']},
      {testcases198, [parallel],
        ['ibm-valid-P86-ibm86v01']},
      {testcases199, [parallel],
        ['ibm-valid-P87-ibm87v01']},
      {testcases200, [parallel],
        ['ibm-valid-P88-ibm88v01']},
      {testcases201, [parallel],
        ['ibm-valid-P89-ibm89v01']},
      {group_ibm_valid_none_xml11, [parallel],
        [{group, testcases202}, {group, testcases203}, {group, testcases204}, {group, testcases205},
         {group, testcases206}, {group, testcases207}, {group, testcases208}]},
      {testcases202, [parallel],
        ['ibm-1-1-valid-P02-ibm02v01', 'ibm-1-1-valid-P02-ibm02v02', 'ibm-1-1-valid-P02-ibm02v03',
         'ibm-1-1-valid-P02-ibm02v04', 'ibm-1-1-valid-P02-ibm02v05']},
      {testcases203, [parallel],
        ['ibm-1-1-valid-P03-ibm03v05', 'ibm-1-1-valid-P03-ibm03v06', 'ibm-1-1-valid-P03-ibm03v07',
         'ibm-1-1-valid-P03-ibm03v08']},
      {testcases204, [parallel],
        ['ibm-1-1-valid-P04-ibm04v01']},
      {testcases205, [parallel],
        ['ibm-1-1-valid-P04-ibm04av01']},
      {testcases206, [parallel],
        ['ibm-1-1-valid-P05-ibm05v01', 'ibm-1-1-valid-P05-ibm05v02', 'ibm-1-1-valid-P05-ibm05v03',
         'ibm-1-1-valid-P05-ibm05v04', 'ibm-1-1-valid-P05-ibm05v05']},
      {testcases207, [parallel],
        ['ibm-1-1-valid-P047-ibm07v01']},
      {testcases208, [parallel],
        ['ibm-1-1-valid-P77-ibm77v07', 'ibm-1-1-valid-P77-ibm77v08', 'ibm-1-1-valid-P77-ibm77v09',
         'ibm-1-1-valid-P77-ibm77v13', 'ibm-1-1-valid-P77-ibm77v14', 'ibm-1-1-valid-P77-ibm77v15']},
      {group_ibm_valid_parameter_, [parallel],
        [{group, testcases209}, {group, testcases210}, {group, testcases211}, {group, testcases212},
         {group, testcases213}, {group, testcases214}, {group, testcases215}, {group, testcases216},
         {group, testcases217}, {group, testcases218}, {group, testcases219}, {group, testcases220},
         {group, testcases221}, {group, testcases222}, {group, testcases223}, {group, testcases224},
         {group, testcases225}, {group, testcases226}, {group, testcases227}, {group, testcases228},
         {group, testcases229}]},
      {testcases209, [parallel],
        ['ibm-valid-P09-ibm09v03', 'ibm-valid-P09-ibm09v05']},
      {testcases210, [parallel],
        ['ibm-valid-P11-ibm11v03', 'ibm-valid-P11-ibm11v04']},
      {testcases211, [parallel],
        ['ibm-valid-P12-ibm12v01', 'ibm-valid-P12-ibm12v02', 'ibm-valid-P12-ibm12v03', 'ibm-valid-P12-ibm12v04']},
      {testcases212, [parallel],
        ['ibm-valid-P13-ibm13v01']},
      {testcases213, [parallel],
        ['ibm-valid-P28-ibm28v02']},
      {testcases214, [parallel],
        ['ibm-valid-P29-ibm29v02']},
      {testcases215, [parallel],
        ['ibm-valid-P30-ibm30v01', 'ibm-valid-P30-ibm30v02']},
      {testcases216, [parallel],
        ['ibm-valid-P31-ibm31v01']},
      {testcases217, [parallel],
        ['ibm-valid-P32-ibm32v01', 'ibm-valid-P32-ibm32v02', 'ibm-valid-P32-ibm32v03', 'ibm-valid-P32-ibm32v04']},
      {testcases218, [parallel],
        ['ibm-valid-P49-ibm49v01']},
      {testcases219, [parallel],
        ['ibm-valid-P50-ibm50v01']},
      {testcases220, [parallel],
        ['ibm-valid-P51-ibm51v02']},
      {testcases221, [parallel],
        ['ibm-valid-P61-ibm61v01', 'ibm-valid-P61-ibm61v02']},
      {testcases222, [parallel],
        ['ibm-valid-P62-ibm62v01', 'ibm-valid-P62-ibm62v02', 'ibm-valid-P62-ibm62v03', 'ibm-valid-P62-ibm62v04',
         'ibm-valid-P62-ibm62v05']},
      {testcases223, [parallel],
        ['ibm-valid-P63-ibm63v01', 'ibm-valid-P63-ibm63v02', 'ibm-valid-P63-ibm63v03', 'ibm-valid-P63-ibm63v04',
         'ibm-valid-P63-ibm63v05']},
      {testcases224, [parallel],
        ['ibm-valid-P64-ibm64v01', 'ibm-valid-P64-ibm64v02', 'ibm-valid-P64-ibm64v03']},
      {testcases225, [parallel],
        ['ibm-valid-P65-ibm65v01', 'ibm-valid-P65-ibm65v02']},
      {testcases226, [parallel],
        ['ibm-valid-P68-ibm68v01']},
      {testcases227, [parallel],
        ['ibm-valid-P69-ibm69v01']},
      {testcases228, [parallel],
        ['ibm-valid-P70-ibm70v01']},
      {testcases229, [parallel],
        ['ibm-valid-P79-ibm79v01']},
      {group_japanese_error_parameter_, [parallel],
        [{group, testcases230}]},
      {testcases230, [parallel],
        ['pr-xml-euc-jp',      'pr-xml-iso-2022-jp', 'pr-xml-shift_jis',   'weekly-euc-jp',      'weekly-iso-2022-jp',
         'weekly-shift_jis']},
      {group_japanese_valid_parameter_, [parallel],
        [{group, testcases231}]},
      {testcases231, [parallel],
        ['pr-xml-little', 'pr-xml-utf-16', 'pr-xml-utf-8',  'weekly-little', 'weekly-utf-16', 'weekly-utf-8']},
      {group_oasis_error_none_, [parallel],
        [{group, testcases232}]},
      {testcases232, [parallel],
        ['o-p11pass1']},
      {group_oasis_invalid_none_, [parallel],
        [{group, testcases233}]},
      {testcases233, [parallel],
        ['o-p01pass1', 'o-p01pass3', 'o-p03pass1', 'o-p04pass1', 'o-p05pass1', 'o-p06fail1', 'o-p08fail1',
         'o-p08fail2', 'o-p10pass1', 'o-p14pass1', 'o-p15pass1', 'o-p16pass1', 'o-p16pass2', 'o-p16pass3',
         'o-p18pass1', 'o-p22pass1', 'o-p22pass2', 'o-p22pass3', 'o-p23pass1', 'o-p23pass2', 'o-p23pass3',
         'o-p23pass4', 'o-p24pass1', 'o-p24pass2', 'o-p24pass3', 'o-p24pass4', 'o-p25pass1', 'o-p25pass2',
         'o-p26pass1', 'o-p27pass1', 'o-p27pass2', 'o-p27pass3', 'o-p27pass4', 'o-p32pass1', 'o-p32pass2',
         'o-p39pass1', 'o-p39pass2', 'o-p40pass1', 'o-p40pass2', 'o-p40pass3', 'o-p40pass4', 'o-p41pass1',
         'o-p41pass2', 'o-p42pass1', 'o-p42pass2', 'o-p44pass1', 'o-p44pass2', 'o-p44pass3', 'o-p44pass4',
         'o-p44pass5', 'o-p66pass1', 'o-p74pass1', 'o-p75pass1', 'o-e2']},
      {group_oasis_not_wf_none_, [parallel],
        [{group, testcases234}]},
      {testcases234, [parallel],
        ['o-p01fail1',  'o-p01fail2',  'o-p01fail3',  'o-p01fail4',  'o-p02fail1',  'o-p02fail10', 'o-p02fail11',
         'o-p02fail12', 'o-p02fail13', 'o-p02fail14', 'o-p02fail15', 'o-p02fail16', 'o-p02fail17', 'o-p02fail18',
         'o-p02fail19', 'o-p02fail2',  'o-p02fail20', 'o-p02fail21', 'o-p02fail22', 'o-p02fail23', 'o-p02fail24',
         'o-p02fail25', 'o-p02fail26', 'o-p02fail27', 'o-p02fail28', 'o-p02fail29', 'o-p02fail3',  'o-p02fail30',
         'o-p02fail31', 'o-p02fail4',  'o-p02fail5',  'o-p02fail6',  'o-p02fail7',  'o-p02fail8',  'o-p02fail9',
         'o-p03fail1',  'o-p03fail10', 'o-p03fail11', 'o-p03fail12', 'o-p03fail13', 'o-p03fail14', 'o-p03fail15',
         'o-p03fail16', 'o-p03fail17', 'o-p03fail18', 'o-p03fail19', 'o-p03fail2',  'o-p03fail20', 'o-p03fail21',
         'o-p03fail22', 'o-p03fail23', 'o-p03fail24', 'o-p03fail25', 'o-p03fail26', 'o-p03fail27', 'o-p03fail28',
         'o-p03fail29', 'o-p03fail3',  'o-p03fail4',  'o-p03fail5',  'o-p03fail7',  'o-p03fail8',  'o-p03fail9',
         'o-p04fail1',  'o-p04fail2',  'o-p04fail3',  'o-p05fail1',  'o-p05fail2',  'o-p05fail3',  'o-p05fail4',
         'o-p05fail5',  'o-p09fail3',  'o-p09fail4',  'o-p09fail5',  'o-p10fail1',  'o-p10fail2',  'o-p10fail3',
         'o-p11fail1',  'o-p11fail2',  'o-p12fail1',  'o-p12fail2',  'o-p12fail3',  'o-p12fail4',  'o-p12fail5',
         'o-p12fail6',  'o-p12fail7',  'o-p14fail1',  'o-p14fail2',  'o-p14fail3',  'o-p15fail1',  'o-p15fail2',
         'o-p15fail3',  'o-p16fail1',  'o-p16fail2',  'o-p16fail3',  'o-p18fail1',  'o-p18fail2',  'o-p18fail3',
         'o-p22fail1',  'o-p22fail2',  'o-p23fail1',  'o-p23fail2',  'o-p23fail3',  'o-p23fail4',  'o-p23fail5',
         'o-p24fail1',  'o-p24fail2',  'o-p25fail1',  'o-p26fail1',  'o-p26fail2',  'o-p27fail1',  'o-p28fail1',
         'o-p29fail1',  'o-p32fail1',  'o-p32fail2',  'o-p32fail3',  'o-p32fail4',  'o-p32fail5',  'o-p39fail1',
         'o-p39fail2',  'o-p39fail3',  'o-p39fail4',  'o-p39fail5',  'o-p40fail1',  'o-p40fail2',  'o-p40fail3',
         'o-p40fail4',  'o-p41fail1',  'o-p41fail2',  'o-p41fail3',  'o-p42fail1',  'o-p42fail2',  'o-p42fail3',
         'o-p43fail1',  'o-p43fail2',  'o-p43fail3',  'o-p44fail1',  'o-p44fail2',  'o-p44fail3',  'o-p44fail4',
         'o-p44fail5',  'o-p45fail1',  'o-p45fail2',  'o-p45fail3',  'o-p45fail4',  'o-p46fail1',  'o-p46fail2',
         'o-p46fail3',  'o-p46fail4',  'o-p46fail5',  'o-p46fail6',  'o-p47fail1',  'o-p47fail2',  'o-p47fail3',
         'o-p47fail4',  'o-p48fail1',  'o-p48fail2',  'o-p49fail1',  'o-p50fail1',  'o-p51fail1',  'o-p51fail2',
         'o-p51fail3',  'o-p51fail4',  'o-p51fail5',  'o-p51fail6',  'o-p51fail7',  'o-p52fail1',  'o-p52fail2',
         'o-p53fail1',  'o-p53fail2',  'o-p53fail3',  'o-p53fail4',  'o-p53fail5',  'o-p54fail1',  'o-p55fail1',
         'o-p56fail1',  'o-p56fail2',  'o-p56fail3',  'o-p56fail4',  'o-p56fail5',  'o-p57fail1',  'o-p58fail1',
         'o-p58fail2',  'o-p58fail3',  'o-p58fail4',  'o-p58fail5',  'o-p58fail6',  'o-p58fail7',  'o-p58fail8',
         'o-p59fail1',  'o-p59fail2',  'o-p59fail3',  'o-p60fail1',  'o-p60fail2',  'o-p60fail3',  'o-p60fail4',
         'o-p60fail5',  'o-p66fail1',  'o-p66fail2',  'o-p66fail3',  'o-p66fail4',  'o-p66fail5',  'o-p66fail6',
         'o-p68fail1',  'o-p68fail2',  'o-p68fail3',  'o-p69fail1',  'o-p69fail2',  'o-p69fail3',  'o-p70fail1',
         'o-p71fail1',  'o-p71fail2',  'o-p71fail3',  'o-p71fail4',  'o-p72fail1',  'o-p72fail2',  'o-p72fail3',
         'o-p72fail4',  'o-p73fail1',  'o-p73fail2',  'o-p73fail3',  'o-p73fail4',  'o-p73fail5',  'o-p74fail1',
         'o-p74fail2',  'o-p74fail3',  'o-p75fail1',  'o-p75fail2',  'o-p75fail3',  'o-p75fail4',  'o-p75fail5',
         'o-p75fail6',  'o-p76fail1',  'o-p76fail2',  'o-p76fail3',  'o-p76fail4']},
      {group_oasis_not_wf_parameter_, [parallel],
        [{group, testcases235}]},
      {testcases235, [parallel],
        ['o-p09fail1', 'o-p09fail2', 'o-p30fail1', 'o-p31fail1', 'o-p61fail1', 'o-p62fail1', 'o-p62fail2',
         'o-p63fail1', 'o-p63fail2', 'o-p64fail1', 'o-p64fail2']},
      {group_oasis_valid_none_, [parallel],
        [{group, testcases236}]},
      {testcases236, [parallel],
        ['o-p01pass2', 'o-p06pass1', 'o-p07pass1', 'o-p08pass1', 'o-p12pass1', 'o-p22pass4', 'o-p22pass5',
         'o-p22pass6', 'o-p28pass1', 'o-p29pass1', 'o-p43pass1', 'o-p45pass1', 'o-p46pass1', 'o-p47pass1',
         'o-p48pass1', 'o-p49pass1', 'o-p50pass1', 'o-p51pass1', 'o-p52pass1', 'o-p53pass1', 'o-p54pass1',
         'o-p55pass1', 'o-p56pass1', 'o-p57pass1', 'o-p58pass1', 'o-p59pass1', 'o-p60pass1', 'o-p68pass1',
         'o-p70pass1', 'o-p71pass1', 'o-p72pass1', 'o-p73pass1', 'o-p76pass1']},
      {group_oasis_valid_parameter_, [parallel],
        [{group, testcases237}]},
      {testcases237, [parallel],
        ['o-p09pass1', 'o-p28pass3', 'o-p28pass4', 'o-p28pass5', 'o-p30pass1', 'o-p30pass2', 'o-p31pass1',
         'o-p31pass2', 'o-p61pass1', 'o-p62pass1', 'o-p63pass1', 'o-p64pass1', 'o-p69pass1']},
      {group_sun_error_none_, [parallel],
        [{group, testcases238}]},
      {testcases238, [parallel],
        [uri01]},
      {group_sun_invalid_none_, [parallel],
        [{group, testcases239}]},
      {testcases239, [parallel],
        ['inv-dtd01',      'inv-dtd02',      'inv-dtd03',      el01,             el02,             el03,
         el04,             el05,             el06,             id04,             id05,             id06,
         id07,             id08,             id09,             'inv-required00', 'inv-required01', 'inv-required02',
         attr01,           attr02,           attr03,           attr04,           attr05,           attr06,
         attr07,           attr08,           attr09,           attr10,           attr11,           attr12,
         attr13,           attr14,           attr15,           attr16,           utf16b,           utf16l,
         empty]},
      {group_sun_invalid_parameter_, [parallel],
        [{group, testcases240}]},
      {testcases240, [parallel],
        [id01,           id02,           id03,           'inv-not-sa01', 'inv-not-sa02', 'inv-not-sa04',
         'inv-not-sa05', 'inv-not-sa06', 'inv-not-sa07', 'inv-not-sa08', 'inv-not-sa09', 'inv-not-sa10',
         'inv-not-sa11', 'inv-not-sa12', 'inv-not-sa13', 'inv-not-sa14', optional01,     optional02,
         optional03,     optional04,     optional05,     optional06,     optional07,     optional08,
         optional09,     optional10,     optional11,     optional12,     optional13,     optional14,
         optional20,     optional21,     optional22,     optional23,     optional24,     optional25,
         root]},
      {group_sun_not_wf_general_, [parallel],
        [{group, testcases241}]},
      {testcases241, [parallel],
        [encoding07]},
      {group_sun_not_wf_none_, [parallel],
        [{group, testcases242}]},
      {testcases242, [parallel],
        [attlist01,   attlist02,   attlist03,   attlist04,   attlist05,   attlist06,   attlist07,   attlist08,
         attlist09,   attlist10,   attlist11,   content01,   content02,   content03,   'nwf-dtd00', 'nwf-dtd01',
         dtd02,       dtd03,       dtd04,       dtd05,       element00,   element01,   element02,   element03,
         element04,   encoding01,  encoding02,  encoding03,  encoding04,  encoding05,  encoding06,  pi,
         pubid01,     pubid02,     pubid03,     pubid04,     pubid05,     sgml01,      sgml02,      sgml03,
         sgml04,      sgml05,      sgml06,      sgml07,      sgml08,      sgml09,      sgml10,      sgml11,
         sgml12,      sgml13]},
      {group_sun_not_wf_parameter_, [parallel],
        [{group, testcases243}]},
      {testcases243, [parallel],
        ['not-wf-sa03', cond01,        cond02,        decl01,        dtd07]},
      {group_sun_valid_general_, [parallel],
        [{group, testcases244}]},
      {testcases244, [parallel],
        [ext01, ext02]},
      {group_sun_valid_none_, [parallel],
        [{group, testcases245}]},
      {testcases245, [parallel],
        [dtd00,      dtd01,      element,    required00, sa01,       sa02,       'v-sgml01', 'v-lang01', 'v-lang02',
         'v-lang03', 'v-lang04', 'v-lang05', 'v-lang06', 'v-pe03']},
      {group_sun_valid_parameter_, [parallel],
        [{group, testcases246}, {group, testcases247}]},
      {testcases246, [parallel],
        [pe01]},
      {testcases247, [parallel],
        ['not-sa01', 'not-sa02', 'not-sa03', 'not-sa04', notation01, optional,   sa03,       sa04,       sa05,
         'v-pe00',   'v-pe02']},
      {group_xmltest_error_both_, [parallel],
        [{group, testcases248}]},
      {testcases248, [parallel],
        ['not-wf-not-sa-005']},
      {group_xmltest_invalid_both_, [parallel],
        [{group, testcases249}, {group, testcases250}]},
      {testcases249, [parallel],
        ['invalid--002', 'invalid--005', 'invalid--006']},
      {testcases250, [parallel],
        ['invalid-not-sa-022']},
      {group_xmltest_not_wf_both_, [parallel],
        [{group, testcases251}]},
      {testcases251, [parallel],
        ['not-wf-not-sa-001', 'not-wf-not-sa-002', 'not-wf-not-sa-003', 'not-wf-not-sa-004', 'not-wf-not-sa-006',
         'not-wf-not-sa-007', 'not-wf-not-sa-008', 'not-wf-not-sa-009', 'not-wf-ext-sa-001', 'not-wf-ext-sa-002',
         'not-wf-ext-sa-003']},
      {group_xmltest_not_wf_general_, [parallel],
        [{group, testcases252}]},
      {testcases252, [parallel],
        ['not-wf-sa-081', 'not-wf-sa-082']},
      {group_xmltest_not_wf_none_, [parallel],
        [{group, testcases253}]},
      {testcases253, [parallel],
        ['not-wf-sa-001', 'not-wf-sa-002', 'not-wf-sa-003', 'not-wf-sa-004', 'not-wf-sa-005', 'not-wf-sa-006',
         'not-wf-sa-007', 'not-wf-sa-008', 'not-wf-sa-009', 'not-wf-sa-010', 'not-wf-sa-011', 'not-wf-sa-012',
         'not-wf-sa-013', 'not-wf-sa-014', 'not-wf-sa-015', 'not-wf-sa-016', 'not-wf-sa-017', 'not-wf-sa-018',
         'not-wf-sa-019', 'not-wf-sa-020', 'not-wf-sa-021', 'not-wf-sa-022', 'not-wf-sa-023', 'not-wf-sa-024',
         'not-wf-sa-025', 'not-wf-sa-026', 'not-wf-sa-027', 'not-wf-sa-028', 'not-wf-sa-029', 'not-wf-sa-030',
         'not-wf-sa-031', 'not-wf-sa-032', 'not-wf-sa-033', 'not-wf-sa-034', 'not-wf-sa-035', 'not-wf-sa-036',
         'not-wf-sa-037', 'not-wf-sa-038', 'not-wf-sa-039', 'not-wf-sa-040', 'not-wf-sa-041', 'not-wf-sa-042',
         'not-wf-sa-043', 'not-wf-sa-044', 'not-wf-sa-045', 'not-wf-sa-046', 'not-wf-sa-047', 'not-wf-sa-048',
         'not-wf-sa-049', 'not-wf-sa-050', 'not-wf-sa-051', 'not-wf-sa-052', 'not-wf-sa-053', 'not-wf-sa-054',
         'not-wf-sa-055', 'not-wf-sa-056', 'not-wf-sa-057', 'not-wf-sa-058', 'not-wf-sa-059', 'not-wf-sa-060',
         'not-wf-sa-061', 'not-wf-sa-062', 'not-wf-sa-063', 'not-wf-sa-064', 'not-wf-sa-065', 'not-wf-sa-066',
         'not-wf-sa-067', 'not-wf-sa-068', 'not-wf-sa-069', 'not-wf-sa-070', 'not-wf-sa-071', 'not-wf-sa-072',
         'not-wf-sa-073', 'not-wf-sa-074', 'not-wf-sa-075', 'not-wf-sa-076', 'not-wf-sa-077', 'not-wf-sa-078',
         'not-wf-sa-079', 'not-wf-sa-080', 'not-wf-sa-083', 'not-wf-sa-084', 'not-wf-sa-085', 'not-wf-sa-086',
         'not-wf-sa-087', 'not-wf-sa-088', 'not-wf-sa-089', 'not-wf-sa-090', 'not-wf-sa-091', 'not-wf-sa-092',
         'not-wf-sa-093', 'not-wf-sa-094', 'not-wf-sa-095', 'not-wf-sa-096', 'not-wf-sa-097', 'not-wf-sa-098',
         'not-wf-sa-099', 'not-wf-sa-100', 'not-wf-sa-101', 'not-wf-sa-102', 'not-wf-sa-103', 'not-wf-sa-104',
         'not-wf-sa-105', 'not-wf-sa-106', 'not-wf-sa-107', 'not-wf-sa-108', 'not-wf-sa-109', 'not-wf-sa-110',
         'not-wf-sa-111', 'not-wf-sa-112', 'not-wf-sa-113', 'not-wf-sa-114', 'not-wf-sa-115', 'not-wf-sa-116',
         'not-wf-sa-117', 'not-wf-sa-118', 'not-wf-sa-119', 'not-wf-sa-120', 'not-wf-sa-121', 'not-wf-sa-122',
         'not-wf-sa-123', 'not-wf-sa-124', 'not-wf-sa-125', 'not-wf-sa-126', 'not-wf-sa-127', 'not-wf-sa-128',
         'not-wf-sa-129', 'not-wf-sa-130', 'not-wf-sa-131', 'not-wf-sa-132', 'not-wf-sa-133', 'not-wf-sa-134',
         'not-wf-sa-135', 'not-wf-sa-136', 'not-wf-sa-137', 'not-wf-sa-138', 'not-wf-sa-139', 'not-wf-sa-142',
         'not-wf-sa-143', 'not-wf-sa-144', 'not-wf-sa-145', 'not-wf-sa-146', 'not-wf-sa-147', 'not-wf-sa-148',
         'not-wf-sa-149', 'not-wf-sa-150', 'not-wf-sa-151', 'not-wf-sa-152', 'not-wf-sa-153', 'not-wf-sa-154',
         'not-wf-sa-155', 'not-wf-sa-156', 'not-wf-sa-157', 'not-wf-sa-158', 'not-wf-sa-159', 'not-wf-sa-160',
         'not-wf-sa-161', 'not-wf-sa-162', 'not-wf-sa-163', 'not-wf-sa-164', 'not-wf-sa-165', 'not-wf-sa-166',
         'not-wf-sa-167', 'not-wf-sa-168', 'not-wf-sa-169', 'not-wf-sa-170', 'not-wf-sa-171', 'not-wf-sa-172',
         'not-wf-sa-173', 'not-wf-sa-174', 'not-wf-sa-175', 'not-wf-sa-176', 'not-wf-sa-177', 'not-wf-sa-178',
         'not-wf-sa-179', 'not-wf-sa-180', 'not-wf-sa-181', 'not-wf-sa-182', 'not-wf-sa-183', 'not-wf-sa-184',
         'not-wf-sa-186']},
      {group_xmltest_not_wf_parameter_, [parallel],
        [{group, testcases254}]},
      {testcases254, [parallel],
        ['not-wf-sa-185']},
      {group_xmltest_valid_both_, [parallel],
        [{group, testcases255}]},
      {testcases255, [parallel],
        ['valid-not-sa-001', 'valid-not-sa-002', 'valid-not-sa-003', 'valid-not-sa-004', 'valid-not-sa-005',
         'valid-not-sa-006', 'valid-not-sa-007', 'valid-not-sa-008', 'valid-not-sa-009', 'valid-not-sa-010',
         'valid-not-sa-011', 'valid-not-sa-012', 'valid-not-sa-013', 'valid-not-sa-014', 'valid-not-sa-015',
         'valid-not-sa-016', 'valid-not-sa-017', 'valid-not-sa-018', 'valid-not-sa-019', 'valid-not-sa-020',
         'valid-not-sa-021', 'valid-not-sa-023', 'valid-not-sa-024', 'valid-not-sa-025', 'valid-not-sa-026',
         'valid-not-sa-027', 'valid-not-sa-028', 'valid-not-sa-029', 'valid-not-sa-030', 'valid-not-sa-031',
         'valid-ext-sa-001', 'valid-ext-sa-002', 'valid-ext-sa-003', 'valid-ext-sa-004', 'valid-ext-sa-005',
         'valid-ext-sa-006', 'valid-ext-sa-007', 'valid-ext-sa-008', 'valid-ext-sa-009', 'valid-ext-sa-011',
         'valid-ext-sa-012', 'valid-ext-sa-013', 'valid-ext-sa-014']},
      {group_xmltest_valid_none_, [parallel],
        [{group, testcases256}]},
      {testcases256, [parallel],
        ['valid-sa-001',  'valid-sa-002',  'valid-sa-003',  'valid-sa-004',  'valid-sa-005',  'valid-sa-006',
         'valid-sa-007',  'valid-sa-008',  'valid-sa-009',  'valid-sa-010',  'valid-sa-011',  'valid-sa-012',
         'valid-sa-013',  'valid-sa-014',  'valid-sa-015',  'valid-sa-016',  'valid-sa-017',  'valid-sa-018',
         'valid-sa-019',  'valid-sa-020',  'valid-sa-021',  'valid-sa-022',  'valid-sa-023',  'valid-sa-024',
         'valid-sa-025',  'valid-sa-026',  'valid-sa-027',  'valid-sa-028',  'valid-sa-029',  'valid-sa-030',
         'valid-sa-031',  'valid-sa-032',  'valid-sa-033',  'valid-sa-034',  'valid-sa-035',  'valid-sa-036',
         'valid-sa-017a', 'valid-sa-037',  'valid-sa-038',  'valid-sa-039',  'valid-sa-040',  'valid-sa-041',
         'valid-sa-042',  'valid-sa-043',  'valid-sa-044',  'valid-sa-045',  'valid-sa-046',  'valid-sa-047',
         'valid-sa-048',  'valid-sa-049',  'valid-sa-050',  'valid-sa-051',  'valid-sa-052',  'valid-sa-053',
         'valid-sa-054',  'valid-sa-055',  'valid-sa-056',  'valid-sa-057',  'valid-sa-058',  'valid-sa-059',
         'valid-sa-060',  'valid-sa-061',  'valid-sa-062',  'valid-sa-063',  'valid-sa-064',  'valid-sa-065',
         'valid-sa-066',  'valid-sa-067',  'valid-sa-068',  'valid-sa-069',  'valid-sa-071',  'valid-sa-072',
         'valid-sa-073',  'valid-sa-074',  'valid-sa-075',  'valid-sa-076',  'valid-sa-077',  'valid-sa-078',
         'valid-sa-079',  'valid-sa-080',  'valid-sa-081',  'valid-sa-082',  'valid-sa-083',  'valid-sa-084',
         'valid-sa-085',  'valid-sa-086',  'valid-sa-087',  'valid-sa-088',  'valid-sa-089',  'valid-sa-090',
         'valid-sa-091',  'valid-sa-092',  'valid-sa-093',  'valid-sa-094',  'valid-sa-095',  'valid-sa-096',
         'valid-sa-098',  'valid-sa-099',  'valid-sa-100',  'valid-sa-101',  'valid-sa-102',  'valid-sa-103',
         'valid-sa-104',  'valid-sa-105',  'valid-sa-106',  'valid-sa-107',  'valid-sa-108',  'valid-sa-109',
         'valid-sa-110',  'valid-sa-111',  'valid-sa-112',  'valid-sa-113',  'valid-sa-114',  'valid-sa-115',
         'valid-sa-116',  'valid-sa-117',  'valid-sa-118',  'valid-sa-119']},
      {group_xmltest_valid_parameter_, [parallel],
        [{group, testcases257}]},
      {testcases257, [parallel],
        ['valid-sa-070', 'valid-sa-097']}].

%%----------------------------------------------------------------------
%% runners
run_test(Config, Base, Uri, Type) ->
    file:set_cwd(datadir(Config)),
    Path = filename:join([datadir(Config), Base, Uri]),
    [
        begin
            R = parse_file(Path, S),
            check_result(R, Type)
        end
     || S <- [1 | lists:seq(2, 20, 2)]
    ].

run_test(Config, Base, Uri, Type, OutUri) ->
    file:set_cwd(datadir(Config)),
    Path = filename:join([datadir(Config), Base, Uri]),
    Out = filename:join([datadir(Config), Base, OutUri]),
    {ok, O} = file:read_file(Out),
    [
        begin
            R = parse_file(Path, S),
            check_result(R, Type, O)
        end
     || S <- [1 | lists:seq(2, 20, 2)]
    ].

%%----------------------------------------------------------------------
%% check_result
check_result({fatal_error, _}, "error") -> {comment, "error on error"};
check_result({ok, _, _}, "error") -> {comment, "recovered from error"};
check_result({ok, _, _}, "invalid") -> {comment, "parsed invalid"};
check_result({fatal_error, _}, "not-wf") -> {comment, "error on not well-formed"};
check_result({ok, _, _}, "valid") -> {comment, "parsed valid"};
check_result(A, T) -> ct:fail({2, T, A}).

check_result({ok, B, _}, "invalid", B) -> {comment, "matched invalid"};
check_result({ok, B, _}, "valid", B) -> {comment, "matched valid"};
check_result({ok, B, _}, "error", B) -> {comment, "matched recovered error"};
check_result({fatal_error, _}, "error", _) -> {comment, "could not recover error"};
check_result(A, T, B) -> ct:fail({3, T, A, B}).

norm_chars(Bin) ->
    <<
        case C of
            9 -> <<"&#9;">>;
            10 -> <<"&#10;">>;
            13 -> <<"&#13;">>;
            $< -> <<"&lt;">>;
            $> -> <<"&gt;">>;
            38 -> <<"&amp;">>;
            $" -> <<"&quot;">>;
            _ -> <<C/utf8>>
        end
     || <<C/utf8>> <= Bin
    >>.

parse_file(Path, Size) ->
    {Cont, Init} = ys_utils:trancoding_file_continuation(Path, Size),
    S = yaccety_sax:stream(Init, [
        {whitespace, true},
        {proc_inst, true},
        {continuation, {Cont, <<>>}},
        {base, filename:dirname(Path)},
        {external, fun ys_utils:external_file_reader/2}
    ]),
    read_write_all(S).

read_write_all(ReadState) ->
    try
        read_write(ReadState, {[], #ys_state{}})
    catch
        _:Err:Stack ->
            {fatal_error, {Err, Stack}}
    end.

read_write(ReadState, WriteState) ->
    case yaccety_sax:next_event(ReadState) of
        {#{type := endDocument} = Event, _} ->
            {Ser, State} = write_event(Event, WriteState),
            {ok, iolist_to_binary(Ser), State};
        {Event, NewReadState} ->
            NewWriteState = write_event(Event, WriteState),
            read_write(NewReadState, NewWriteState)
    end.

write_event(
    #{type := startElement, qname := QName, attributes := Attributes},
    {Output, WriteState}
) ->
    Atts = [
        [<<" ">>, qname(AQName), <<"=\"">>, norm_chars(iolist_to_binary(V)), <<"\"">>]
     || {AQName, V} <- lists:sort(Attributes)
    ],
    Tag = [<<"<">>, qname(QName), Atts, <<">">>],
    {[Output, Tag], WriteState};
write_event(#{type := endElement, qname := QName}, {Output, WriteState}) ->
    Tag = [<<"</">>, qname(QName), <<">">>],
    {[Output, Tag], WriteState};
write_event(#{type := characters, data := Data}, {Output, WriteState}) ->
    Chars = norm_chars(Data),
    {[Output, Chars], WriteState};
write_event(#{type := processingInstruction, target := Target, data := Data}, {Output, WriteState}) ->
    PI =
        case iolist_to_binary(Data) of
            <<>> -> [<<"<?">>, Target, <<" ?>">>];
            Data1 -> [<<"<?">>, Target, <<" ">>, Data1, <<"?>">>]
        end,
    {[Output, PI], WriteState};
% Return the notations sorted by name.
write_event(
    #{type := dtd, proc := #{nots := Nots, name := DName, pi_comments := Pis}}, {Output, WriteState}
) when
    is_map(Nots)
->
    ProcInst = [
        #{type => processingInstruction, target => Target, data => Data}
     || {pi, Target, Data} <- Pis
    ],
    {Output0, WriteState0} = lists:foldl(fun write_event/2, {Output, WriteState}, ProcInst),
    case maps:size(Nots) of
        0 ->
            {Output0, WriteState0};
        _ ->
            NotList = lists:sort(maps:to_list(Nots)),
            F = fun
                ({Name, {<<>>, Sys}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" SYSTEM '">>, Sys, <<"'>">>];
                ({Name, {Pub, <<>>}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" PUBLIC '">>, Pub, <<"'>">>];
                ({Name, {Pub, Sys}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" PUBLIC '">>, Pub, <<"' '">>, Sys, <<"'>">>]
            end,
            IoNots = [F(N) || N <- NotList],
            DTD = [<<"<!DOCTYPE ">>, qname(DName), <<" [">>, IoNots, <<"\n]>\n">>],
            {[Output0, DTD], WriteState0}
    end;
write_event(_Event, {Output, WriteState}) ->
    {Output, WriteState}.

qname({<<>>, Ln}) -> Ln;
qname({_, <<>>, Ln}) -> Ln;
qname({_, Px, Ln}) -> <<Px/binary, $:, Ln/binary>>.

%%======================================================================
%% Support Functions
%%======================================================================

%% Dir is a directory
rm_f_(Dir) ->
    {ok, CWD} = file:get_cwd(),
    {ok, FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD, Dir])),
    rm_files(FileList),
    file:set_cwd(CWD),
    ok = file:del_dir(Dir).

rm_files([]) ->
    ok;
rm_files([F | Fs]) ->
    case filelib:is_dir(F) of
        true ->
            rm_f_(F);
        _ ->
            ok = file:delete(F)
    end,
    rm_files(Fs).

change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir) ->
    {ok, CWD} = file:get_cwd(),
    {ok, FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD, Dir])),
    change_mode3(FileList),
    file:set_cwd(CWD).
change_mode3([]) ->
    ok;
change_mode3([F | Fs]) ->
    case filelib:is_dir(F) of
        true ->
            chmod(F),
            change_mode2(F);
        _ ->
            chmod(F)
    end,
    change_mode3(Fs).

chmod(F) ->
    case file:read_file_info(F) of
        {ok, FileInfo} ->
            Mode = FileInfo#file_info.mode,
            file:write_file_info(F, FileInfo#file_info{mode = 8#00777 bor Mode});
        _ ->
            ok
    end.

privdir(Config) ->
    proplists:get_value(priv_dir, Config).
datadir(Config) ->
    proplists:get_value(data_dir, Config).
