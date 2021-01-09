-record(ys_state, {
    position = [document],
    stream_offset = {<<>>, 0},
    inscope_ns = [none],
    tags = [],
    line = 1,
    whitespace = false,
    comments = true,
    proc_inst = true,
    continuation = undefined,
    base = undefined,
    standalone = false,
    dtd = undefined
}).

-type qname() :: {
    % Namespace URI of this QName.
    NamespaceUri :: binary(),
    % Prefix of this QName.
    Prefix :: binary(),
    % Local part of this QName.
    LocalPart :: binary()
}.

-type location() :: pos_integer().

-type dtd_type() ::
    cdata
    | id
    | idref
    | idrefs
    | entity
    | entities
    | nmtoken
    | nmtokens
    | {notation, list(binary())}
    | {enum, list(binary())}.

-type processed_dtd() :: #{
    external := {Pub :: binary(), Sys :: binary()},
    elements := #{Name :: binary() := empty | any | mixed | {mixed, [binary()]}},
    attributes := #{Elem :: binary() := [term()]},
    notations := #{Name :: binary() := {binary(), binary()}},
    references := #{Name :: binary() := term()},
    parameters := #{Name :: binary() := binary()}
}.

-type xml_dtd() :: #{
    type := dtd,
    line := location(),
    % Entire Document Type Declaration as a string
    text := binary(),
    % Representation of the DTD
    proc := undefined | processed_dtd()
}.

-type xml_startDocument() :: #{
    type := startDocument,
    line := location(),
    % The version of XML of this XML stream
    version := binary(),
    % The encoding of the XML data
    encoding := binary(),
    % true if Encoding was set in the encoding declaration of the document
    enc_set := boolean(),
    % true if XML is standalone
    standalone := boolean(),
    % true if the standalone attribute was set in the encoding declaration of the document.
    sa_set := boolean()
}.

-type xml_endDocument() :: #{
    type := endDocument,
    line := location()
}.

-type xml_startElement() :: #{
    type := startElement,
    line := location(),
    % QName for this element.
    qname := qname(),
    % The attributes if any
    attributes := list(xml_attribute()),
    % The namespaces if any
    namespaces := list(xml_namespace())
}.

-type xml_endElement() :: #{
    type := endElement,
    line := location(),
    % QName for this event.
    qname := qname()
}.

-type xml_attribute() :: #{
    type := attribute,
    line := location(),
    % QName for this attribute.
    qname := qname(),
    % The normalized value of this attribute.
    value := binary(),
    % The type of this attribute, default is cdata.
    dtd_type => dtd_type(),
    % A flag indicating whether this attribute was actually specified in the start-tag of its element, or was defaulted from the schema.
    specified => boolean()
}.

-type xml_characters() :: #{
    type := characters,
    line := location(),
    % The character data
    data := binary(),
    % true if this is a CData section.
    cdata := boolean(),
    % true if this is ignorableWhiteSpace.
    ignorable := boolean(),
    % true if this set of Characters is all whitespace.
    space := boolean()
}.

-type xml_comment() :: #{
    type := comment,
    line := location(),
    % Comment text
    text := binary()
}.

-type xml_processingInstruction() :: #{
    type := processingInstruction,
    line := location(),
    % The target section of the processing instruction
    target := binary(),
    % The data section of the processing instruction
    data := binary()
}.

-type xml_namespace() :: #{
    type := namespace,
    line := location(),
    % The uri bound to the prefix of this namespace
    uri := binary(),
    % The prefix, <<>> if this is a default namespace declaration.
    prefix := binary()
}.

% unparsed entity declarations
-type xml_entityDeclaration() ::
    {Type :: entityDeclaration, Location :: location(),
        % Base URI for this reference or `undefined` if this information is not available
        BaseURI :: undefined | binary(),
        % The entity's name.
        Name :: binary(),
        % The name of the associated notation.
        NotationName :: binary(),
        % The entity's public identifier, or `undefined` if none was given
        PublicId :: undefined | binary(),
        % The replacement text of the entity.
        ReplText :: binary(),
        % The entity's system identifier.
        SystemId :: binary()}.

-type xml_entityReference() ::
    {Type :: entityReference, Location :: location(),
        % The declaration of this entity.
        Declaration :: xml_entityDeclaration(),
        % The entity's name.
        Name :: binary()}.

-type xml_notationDeclaration() ::
    {Type :: notationDeclaration, Location :: location(),
        % The notation's name.
        Name :: binary(),
        % The notation's public identifier, or `undefined` if none was given
        PublicId :: undefined | binary(),
        % The notation's system identifier, or `undefined` if none was given
        SystemId :: undefined | binary()}.

-type xml_event() ::
    xml_characters()
    | xml_comment()
    | xml_dtd()
    | xml_endDocument()
    | xml_endElement()
    | xml_entityReference()
    | xml_processingInstruction()
    | xml_startDocument()
    | xml_startElement().
