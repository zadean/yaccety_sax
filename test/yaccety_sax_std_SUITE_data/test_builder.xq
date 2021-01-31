declare variable $xmlconf := doc('xmlconf.xml');

declare variable $skip := 
  map{
    "'not-wf-not-sa-005'" : 'unknown parameter reference in external (VC test not WFC)',
    "'not-wf-ext-sa-001'" : 'recursive external reference',
    "'rmt-ns10-004'"      : 'Namespace name test: a relative URI (deprecated)',
    "'rmt-ns10-005'"      : 'Namespace name test: a same-document relative URI (deprecated)',
    "'valid-sa-012'"      : 'Uses a legal XML 1.0 name consisting of a single colon character (disallowed by the latest XML Namespaces draft).',
    "'rmt-e2e-50'"        : 'XML 1.1 EOL Char',
    "'x-ibm-1-0.5-valid-P04-ibm04v01'" : 'Non-namespaced test',
    "'x-ibm-1-0.5-valid-P05-ibm05v01'" : 'Non-namespaced test',
    "'x-ibm-1-0.5-valid-P05-ibm05v02'" : 'Non-namespaced test',
    "'x-ibm-1-0.5-valid-P05-ibm05v03'" : 'Non-namespaced test',
    "'x-ibm-1-0.5-valid-P05-ibm05v05'" : 'Non-namespaced test',
    "'o-p04pass1'" : 'Non-namespaced test',
    "'o-p05pass1'" : 'Non-namespaced test',
    "'hst-lhs-007'": 'Non-matched file encoding and declaration, app only sees UTF-8',
    "'hst-lhs-008'": 'Non-matched file encoding and declaration, app only sees UTF-8',
    "'rmt-e2e-61'": 'Non-matched file encoding and declaration, app only sees UTF-8',
    "'rmt-e2e-38'": 'E10 v4e: allow parsers to attempt forward-compatible processing of post-1.0 documents',
    (: PROBLEM TESTS - each inconsistent with other tests :)
    "'ibm-not-wf-P32-ibm32n09'": 'ISSUE: Violates standalone=yes',
    "'ibm-not-wf-P68-ibm68n06'": 'ISSUE: Violates standalone=yes',
    "'not-wf-sa03'": 'ISSUE: Violates standalone=yes',
    "'ibm-not-wf-p28a-ibm28an01'": 'ISSUE: Violates WFC:PE Between Declarations in Production 28a'
  };

declare variable $head := 
'%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
%% File    : yaccety_sax_std_SUITE.erl
%% Created : ' || current-date() => adjust-date-to-timezone(()) || '
%%----------------------------------------------------------------------
-module(yaccety_sax_std_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("yaccety_sax.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------
';

declare variable $support :=
``[
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

rm_files([])->
    ok;
rm_files([F|Fs]) ->
    case filelib:is_dir(F) of
        true ->
            rm_f_(F);
        _ ->
            ok = file:delete(F)
    end,
    rm_files(Fs).


change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
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
]``;
declare variable $foot :=
'

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
            9 -> <<"&amp;#9;">>;
            10 -> <<"&amp;#10;">>;
            13 -> <<"&amp;#13;">>;
            $< -> <<"&amp;lt;">>;
            $> -> <<"&amp;gt;">>;
            38 -> <<"&amp;amp;">>;
            $" -> <<"&amp;quot;">>;
            _ -> <<C/utf8>>
        end
        || <<C/utf8>> <= Bin
    >>.

parse_file(Path) ->
    {Cont, Init} = ys_utils:trancoding_file_continuation(Path),
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
write_event(#{type := dtd, proc := #{nots := Nots, name := DName, pi_comments := Pis}}, {Output, WriteState}) when
    is_map(Nots)
->
    ProcInst = [#{type => processingInstruction, target => Target, data => Data} || {pi, Target, Data} <- Pis],
    {Output0, WriteState0} = lists:foldl(fun write_event/2, {Output, WriteState}, ProcInst),
    case maps:size(Nots) of
        0 ->
            {Output0, WriteState0};
        _ ->
            NotList = lists:sort(maps:to_list(Nots)),
            F = fun
                ({Name, {<<>>, Sys}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" SYSTEM ''">>, Sys, <<"''>">>];
                ({Name, {Pub, <<>>}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" PUBLIC ''">>, Pub, <<"''>">>];
                ({Name, {Pub, Sys}}) ->
                    [<<"\n<!NOTATION ">>, Name, <<" PUBLIC ''">>, Pub, <<"'' ''">>, Sys, <<"''>">>]
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
';


declare function local:skip($id)
{
  $skip($id) => empty() => not()
};

declare function local:normalize-comment($test)
{
  $test/text() => string-join(' ')
    => normalize-space() 
    => tokenize() 
    => local:normalize-comment('%%  ', '%% Description:')
};

declare function local:normalize-comment($words, $acc, $lines)
{
  if (empty($words)) then
    ($lines, $acc) => string-join('&#10;')
  else
    let $nextword := $words => head()
    let $posslength := ($nextword => string-length()) + ($acc => string-length()) + 1
    return
    if ( $posslength > 73 ) then
      local:normalize-comment($words, '%%  ', ($lines, $acc) )
    else
      local:normalize-comment(tail($words), $acc || ' ' || $nextword, $lines )
};

declare function local:table-join($strings, $sep)
{
  let $width := ($strings ! string-length(.)) => max()
  let $pad := function($str)
              {
                $str || ',' || ((1 to ($width - string-length($str))) ! ' ') => string-join() 
              }
  let $str0 := reverse($strings)
  let $strs1 := (head($str0), tail($str0) ! $pad(.)) => reverse()
  let $strs2 := 
    for tumbling window $w in $strs1
    start at $s when fn:true()
    end at $e when $e - $s eq (110 idiv ($width + 2)) -1
    return $w => string-join(' ')
  return
    $strs2 => string-join($sep)
};

declare function local:id-atom($test)
{
  let $s0 := $test/@ID => string()
  let $s1 := $s0 => replace('\.xml', '')  
  return
  if(contains($s1, '-')) then
    "'" || $s1 || "'"
  else
    $s1
};

declare function local:edition($test)
{
  if($test/@EDITION) then
  '%% Edition: ' || $test/@EDITION => string()
  else ()
};

declare function local:entities($test)
{
  if($test/@ENTITIES and $test/@ENTITIES ne 'none') then
  '%% Entities: ' || $test/@ENTITIES => string()
  else ()
  
};

declare function local:output($test)
{
  if($test/@OUTPUT) then
  '%% Output: ' || $test/@OUTPUT => string()
  else ()
};

declare function local:output-uri($test)
{
  $test/@OUTPUT => string()
};

declare function local:has-ouput($test)
{
  $test/@OUTPUT => boolean()
};

declare function local:test-case($test, $base)
{
  let $uri := $test/@URI => string()
  let $type := $test/@TYPE => string()
  let $out := local:has-ouput($test)
  let $id := local:id-atom($test)
  let $s := local:skip($id)
  let $st := if($s) then '(_Config) -> {skip, "'||$skip($id)||'"}.' else '(Config) -> '
  let $ct := if($s) then '%%' else ''
  return
  (
    '%%----------------------------------------------------------------------',
    '%% ID: '       || $test/@ID => string(),
    (: local:edition($test), :)
    local:entities($test),
    local:output($test),
    local:normalize-comment($test),
    $id|| $st || (
      if($out) then
        $ct||' run_test(Config, "'|| $base||'", "'||$uri||'", "'||$type||'", "'||local:output-uri($test)||'").'
      else 
        $ct||' run_test(Config, "'|| $base||'", "'||$uri||'", "'||$type||'").'
    )
  ) => string-join('&#10;')

||'&#10;'
};
(:
TYPE - response type
ENTITIES - none, general, parameter, both
EDITION - (1 2 3 4) | 5
NAMESPACE - no | yes
RECOMMENDATION - XML1.1 | XML1.0-errata2e | NS1.0 | NS1.1 | XML1.0-errata3e | XML1.0-errata4e | NS1.0-errata1e
  :)
declare function local:recommendation($n)
{
  (
    $n/@RECOMMENDATION
    => replace('\.', '')
    => replace('-', '_')
    => lower-case()
    ,
    'xml10'
  )[1]
};

declare function local:group-tests()
{
  for $tcs1 in $xmlconf/TESTSUITE/TESTCASES
  let $base := $tcs1/@xml:base => substring-before('/')
  (: where $base = ('xmltest', 'japanese', 'sun', 'oasis', 'ibm', 'eduni') :)
  for $test in $tcs1//TEST[not(@EDITION eq '1 2 3 4')]
  let $p := $test/../@PROFILE
  let $id := local:id-atom($test)
  let $ent := $test/@ENTITIES
  (: where ($ent, 'none')[1] = 'none' :)
  let $grp := ($base,$test/@TYPE => replace('-','_'),if($ent and $ent ne 'none') then $ent else 'none', local:recommendation($test)) => string-join('_') 
  group by
    $base := $grp
    , $p
    , $out := $test/@OUTPUT => boolean()
  order by
    $base
  count $pos
  return
    [$pos, $base, $id, $p]
};

declare function local:groups-fun()
{
  '%% erlfmt-ignore&#10;'||
  'groups() -> &#10;    [&#10;' ||
  (
    for $g in local:group-tests()
    let $grp := 'group_' || $g(2)
    let $tc := 'testcases' || $g(1)
    let $tcs := '      {'||$tc||', [parallel], &#10;        ['||$g(3) => local:table-join('&#10;         ')||']}'
    group by
      $grp
    order by
      $grp
    return
    '      {'||$grp||', [parallel], &#10;        ['||($tc ! ('{group, '||.||'}') ) 
      => local:table-join('&#10;         ')||']},&#10;' ||
    $tcs => string-join(',&#10;')
  ) => string-join(',&#10;')
  || '].'  
};

declare function local:all-fun()
{
  let $grp := (local:group-tests() ! .(2)) => distinct-values() => sort()
  return
``[all() ->
    [
      `{ $grp ! ((if(contains(., 'xml11') or contains(., 'ns11')) then '%' else '') || '{group, group_'||.||'}') => string-join(',&#10;      ') }`
    ].
]``
};

declare function local:inits()
{
``[
%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

-define(dont_rm_test_dirs, true).

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
]``  
};

declare function local:null()
{
  (
    $head,
    (
      for $tcs1 in $xmlconf/TESTSUITE/TESTCASES
      let $pro1 := $tcs1/@PROFILE => string()
      let $base := $tcs1/@xml:base => replace('/$', '')
      return
      '%%----------------------------------------------------------------------' || '&#10;' ||
      '%% Test Cases' || '&#10;' ||
      '%% Profile: ' || $pro1 || '&#10;' ||
      '%%----------------------------------------------------------------------' || '&#10;' ||
      ' ' || '&#10;' ||
      (
        for $tcs2 in $tcs1//TEST[not(@EDITION eq '1 2 3 4')]
        (: [(@ENTITIES, 'none')[1] = 'none'  ] :)
        return $tcs2 => local:test-case($base)
      ) => string-join('&#10;')
    ),
    local:inits(),
    local:all-fun(),
    local:groups-fun(),
    $foot,
    $support
  )
  => string-join('&#10;')
};

file:write-text(file:base-dir() || '../yaccety_sax_std_SUITE.erl', local:null())

