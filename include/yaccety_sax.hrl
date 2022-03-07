-record(ys_state, {
    rest_stream = <<>>,
    position = [document],
    tags = [],
    inscope_ns = [none],
    line = 1,
    continuation = undefined,
    base = undefined,
    dtd = undefined,
    in_replacement = false,
    whitespace = false,
    comments = true,
    proc_inst = true,
    standalone = false,
    namespace_aware = true,
    external = undefined
}).

-type qname() ::
    {
        % Prefix of this QName.
        Prefix :: binary(),
        % Local part of this QName.
        LocalPart :: binary()
    }
    | {
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
    % Root element name
    name := undefined | qname(),
    % External DTD IDs
    external_id := undefined | {Pub :: binary(), Sys :: binary()},
    % Element declarations
    elems := #{Name :: binary() := empty | any | mixed | {mixed, [binary()]}},
    % Attribute List declarations
    atts := #{
        {ElemPx :: binary(), ElemLn :: binary()} := #{
            {AttPx :: binary(), AttLn :: binary()} := term()
        }
    },
    % Notation declarations
    nots := #{Name :: binary() := {binary(), binary()}},
    % Entities used in the document content
    gen_ents := #{Name :: binary() := binary()},
    % Entities used in the DTD
    par_ents := #{Name :: binary() := binary()},
    % Stack of named references
    ref_stack := list(),
    % Processing Instructions and comments from the DTD
    pi_comments := list()
}.

-type xml_dtd() :: #{
    type := dtd,
    line := location(),
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

-type xml_attribute() :: {
    % Attribute QName.
    QName :: qname(),
    % The normalized value of this attribute.
    Value :: binary()
}.

-type xml_namespace() :: {
    % The uri bound to the prefix of this namespace
    Uri :: binary(),
    % The prefix, <<>> if this is a default namespace declaration.
    Prefix :: binary()
}.

-type xml_event() ::
    xml_characters()
    | xml_comment()
    | xml_dtd()
    | xml_endDocument()
    | xml_endElement()
    | xml_processingInstruction()
    | xml_startDocument()
    | xml_startElement().
