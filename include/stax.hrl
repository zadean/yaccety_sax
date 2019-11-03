
-type qname() :: 
    {NamespaceUri :: binary(), % Namespace URI of this QName.
     Prefix :: binary(),       % Prefix of this QName.
     LocalPart :: binary()     % Local part of this QName.
    }.

-type location() :: pos_integer().

-type dtd_type() :: 
    cdata | id | idref | idrefs | entity | entities | nmtoken | nmtokens |
    {notation, list(binary())} | {enum, list(binary())}.


-type processed_dtd() :: 
    #{external      := {Pub :: binary(), Sys :: binary()},
      elements      := #{Name :: binary() := empty | any | mixed | {mixed, [binary()]}},
      attributes    := #{Elem :: binary() := [term()]},
      notations     := #{Name :: binary() := {binary(), binary()}},
      references    := #{Name :: binary() := term()},
      parameters    := #{Name :: binary() := binary()}
     }.

-type xml_dtd() :: 
    #{type := dtd,
      line := location(),
      text := binary(),                     % Entire Document Type Declaration as a string
      proc := undefined | processed_dtd()   % Representation of the DTD
     }.

-type xml_startDocument() :: 
    #{type       := startDocument,
      line       := location(),
      version    := binary(),   % The version of XML of this XML stream
      encoding   := binary(),   % The encoding of the XML data
      enc_set    := boolean(),  % true if Encoding was set in the encoding declaration of the document
      standalone := boolean(),  % true if XML is standalone
      sa_set     := boolean()   % true if the standalone attribute was set in the encoding declaration of the document.
     }.

-type xml_endDocument() :: 
    #{type  := endDocument,
      line  := location()
     }.

-type xml_startElement() :: 
    #{type       := startElement,
      line       := location(),
      qname      := qname(),               % QName for this element.
      attributes := list(xml_attribute()), % The attributes if any
      namespaces := list(xml_namespace())  % The namespaces if any
     }.

-type xml_endElement() :: 
    #{type  := endElement,
      line  := location(),
      qname := qname()      % QName for this event.
     }.

-type xml_attribute() :: 
    #{type      := attribute,
      line      := location(),
      qname     := qname(),     % QName for this attribute.
      value     := binary(),    % The normalized value of this attribute.
      dtd_type  => dtd_type(),  % The type of this attribute, default is cdata.
      specified => boolean()    % A flag indicating whether this attribute was actually specified in the start-tag of its element, or was defaulted from the schema.
     }.

-type xml_characters() :: 
    #{type      := characters,
      line      := location(),
      data      := binary(),   % The character data
      cdata     := boolean(),  % true if this is a CData section.
      ignorable := boolean(),  % true if this is ignorableWhiteSpace.
      space     := boolean()   % true if this set of Characters is all whitespace.
     }.

-type xml_comment() :: 
    #{type  := comment,
      line  := location(),
      text  := binary()     % Comment text
     }.

-type xml_processingInstruction() :: 
    #{type   := processingInstruction,
      line   := location(),
      target := binary(),   % The target section of the processing instruction
      data   := binary()    % The data section of the processing instruction
     }.

-type xml_namespace() :: 
    #{type      := namespace,
      line      := location(),
      uri       := binary(),    % The uri bound to the prefix of this namespace
      prefix    := binary()     % The prefix, <<>> if this is a default namespace declaration.
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
-type xml_notationDeclaration() :: 
    {Type :: notationDeclaration,
     Location :: location(),
     Name :: binary(),                 % The notation's name.
     PublicId :: undefined | binary(), % The notation's public identifier, or `undefined` if none was given
     SystemId :: undefined | binary()  % The notation's system identifier, or `undefined` if none was given
    }.


-type xml_event() :: 
    xml_attribute() | xml_characters() | xml_comment() | xml_dtd() |
    xml_endDocument() | xml_endElement() | xml_entityDeclaration() |
    xml_entityReference() | xml_namespace() | xml_notationDeclaration() |
    xml_processingInstruction() | xml_startDocument() | xml_startElement().


