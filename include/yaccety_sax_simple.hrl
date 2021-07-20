-record(ys_state_simple, {
    rest_stream = <<>>,
    position = [document],
    tags = [],
    inscope_ns = [none]
}).

-type qname() :: {
    % Namespace URI of this QName.
    NamespaceUri :: binary(),
    % Prefix of this QName.
    Prefix :: binary(),
    % Local part of this QName.
    LocalPart :: binary()
}.

-type xml_startDocument() :: {
    startDocument,
    % The version of XML of this XML stream
    Version :: binary(),
    % The encoding of the XML data
    Encoding :: binary()
}.

-type xml_endDocument() :: endDocument.

-type xml_startElement() :: {
    startElement,
    % QName for this element.
    QName :: qname(),
    % The attributes if any
    Attributes :: list(xml_attribute()),
    % The namespaces if any
    Namespaces :: list(xml_namespace())
}.

-type xml_endElement() :: {
    endElement,
    % QName for this event.
    QName :: qname()
}.

-type xml_characters() :: {
    characters,
    % The character data
    Data :: binary()
}.

-type xml_attribute() :: {
    % The prefix for this attribute's QName
    Px :: binary(),
    % The local name for this attribute's QName
    Ln :: binary(),
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
    | xml_endDocument()
    | xml_endElement()
    | xml_startDocument()
    | xml_startElement().
