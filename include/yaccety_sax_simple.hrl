% Event stack positions
-define(document, 0).
-define(content, 1).
-define(element, 2).
-define(empty, 3).
-define(misc_pre_dtd, 4).
-define(misc_post_dtd, 5).
-define(dtd, 6).
-define(misc_post_element, 7).

-record(ys_state_simple, {
    rest_stream = <<>>,
    position = [?document],
    tags = []
}).

-type qname() ::
    % Prefix and Local part of this QName.
    PrefixLocalPart :: binary().

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
    AttributesNamespaces :: list(xml_attribute())
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
    % The Name
    Name :: qname(),
    % The normalized value of this attribute.
    Value :: binary()
}.

-type xml_event() ::
    xml_characters()
    | xml_endDocument()
    | xml_endElement()
    | xml_startDocument()
    | xml_startElement().
