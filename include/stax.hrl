-type qname() :: 
    {NamespaceUri :: binary(), % Namespace URI of this QName.
     LocalPart :: binary(),    % Local part of this QName.
     Prefix :: binary()        % Prefix of this QName.
    }.
-type location() :: pos_integer().
-type dtd_type() :: 
    cdata | id | idref | idrefs | entity | entities | nmtoken | nmtokens |
    {notation, list(binary())} | {enum, list(binary())}.
-type xml_attribute() :: 
    {Type :: attribute,
     Location :: location(),
     QName :: qname(),      % QName for this attribute.
     Value :: binary(),     % The normalized value of this attribute.
     DTDType :: dtd_type(), % The type of this attribute, default is cdata.
     Specified :: boolean() % A flag indicating whether this attribute was actually specified in the start-tag of its element, or was defaulted from the schema.
    }.
-type xml_characters() :: 
    #{type   := characters,
      line   := Location :: location(),
      data   := Data :: binary(),       % The character data
      cdata  := CData :: boolean(),     % true if this is a CData section.
      ignore := Ignorable :: boolean(), % true if this is ignorableWhiteSpace.
      ws     := WhiteSpace :: boolean() % true if this set of Characters is all whitespace.
     }.
-type xml_comment() :: 
    #{type := comment,
      line := Location :: location(),
      text := Text :: binary() % Comment text
    }.

-type proc_dtd() :: 
    #{elems := #{Name :: binary() := empty | any | mixed | {mixed, [binary()]}},
      atts := #{Elem :: binary() := [term()]},
      nots := #{Name :: binary() := {binary(), binary()}},
      refs := #{Name :: binary() := term()},
      params := #{Name :: binary() := binary()}
    }.

-type xml_dtd() :: 
    #{type := dtd,
      line := Location :: location(),
      external := Name :: binary(),
      text := Text :: binary(),    % Entire Document Type Declaration as a string
      proc := Processed :: undefined | proc_dtd()  % Representation of the DTD
    }.
-type xml_endDocument() :: 
    {Type :: endDocument,
     Location :: location()
    }.
-type xml_endElement() :: 
    #{type  := endElement,
      line  := Location :: location(),
      qname := QName :: qname()    % QName for this event.
    }.
% unparsed entity declarations
-type xml_entityDeclaration() :: 
    {Type :: entityDeclaration,
     Location :: location(),
     BaseURI :: undefined | binary(),  % Base URI for this reference or `undefined` if this information is not available
     Name :: binary(),                 % The entity's name.
     NotationName :: binary(),         % The name of the associated notation.
     PublicId :: undefined | binary(), % The entity's public identifier, or `undefined` if none was given
     ReplText :: binary(),             % The replacement text of the entity.
     SystemId :: binary()              % The entity's system identifier.
    }.
-type xml_entityReference() :: 
    {Type :: entityReference,
     Location :: location(),
     Declaration :: xml_entityDeclaration(), % The declaration of this entity.
     Name :: binary()                        % The entity's name.
    }.
-type xml_namespace() :: 
    {Type :: namespace,
     Location :: location(),
     URI :: binary(),     % The uri bound to the prefix of this namespace
     Prefix :: binary(),  % The prefix, <<>> if this is a default namespace declaration.
     Default :: boolean() % true if this attribute declares the default namespace
    }.
-type xml_notationDeclaration() :: 
    {Type :: notationDeclaration,
     Location :: location(),
     Name :: binary(),                 % The notation's name.
     PublicId :: undefined | binary(), % The notation's public identifier, or `undefined` if none was given
     SystemId :: undefined | binary()  % The notation's system identifier, or `undefined` if none was given
    }.
-type xml_processingInstruction() :: 
    #{type   := processingInstruction,
      line   := Location :: location(),
      target := Target :: binary(), % The target section of the processing instruction
      data   := Data :: binary()    % The data section of the processing instruction
    }.
-type xml_startDocument() :: 
    #{type       := startDocument,
      line       := Location :: location(),
      version    := Version :: binary(),     % The version of XML of this XML stream
      encoding   := Encoding :: binary(),    % The encoding of the XML data
      enc_set    := EncSet :: boolean(),     % true if Encoding was set in the encoding declaration of the document
      standalone := StandAlone :: boolean(), % true if XML is standalone
      sa_set     := StandSet :: boolean()    % true if the standalone attribute was set in the encoding declaration of the document.
    }.
-type xml_startElement() :: 
    #{type       := startElement,
      line       := Location   :: location(),
      qname      := QName      :: qname(),               % QName for this element.
      attributes := Attributes :: list(xml_attribute()), % The attributes if any
      namespaces := Namespaces :: list(xml_namespace())  % The namespaces if any
    }.


-type xml_event() :: 
    xml_attribute() | xml_characters() | xml_comment() | xml_dtd() |
    xml_endDocument() | xml_endElement() | xml_entityDeclaration() |
    xml_entityReference() | xml_namespace() | xml_notationDeclaration() |
    xml_processingInstruction() | xml_startDocument() | xml_startElement().


