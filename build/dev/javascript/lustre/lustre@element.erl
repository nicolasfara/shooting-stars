-module(lustre@element).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([element/3, namespaced/4, text/1, map/2, to_string_builder_helper/2, to_string/1, to_string_builder/1]).
-export_type([element/1]).

-opaque element(FNF) :: {text, binary()} |
    {element,
        binary(),
        list(lustre@attribute:attribute(FNF)),
        list(element(FNF))} |
    {element_ns,
        binary(),
        list(lustre@attribute:attribute(FNF)),
        list(element(FNF)),
        binary()}.

-spec element(
    binary(),
    list(lustre@attribute:attribute(FNG)),
    list(element(FNG))
) -> element(FNG).
element(Tag, Attrs, Children) ->
    {element, Tag, Attrs, Children}.

-spec namespaced(
    binary(),
    binary(),
    list(lustre@attribute:attribute(FNM)),
    list(element(FNM))
) -> element(FNM).
namespaced(Namespace, Tag, Attrs, Children) ->
    {element_ns, Tag, Attrs, Children, Namespace}.

-spec text(binary()) -> element(any()).
text(Content) ->
    {text, Content}.

-spec escape(binary(), binary()) -> binary().
escape(Escaped, Content) ->
    case Content of
        <<"<"/utf8, Rest/binary>> ->
            escape(<<Escaped/binary, "&lt;"/utf8>>, Rest);

        <<">"/utf8, Rest@1/binary>> ->
            escape(<<Escaped/binary, "&gt;"/utf8>>, Rest@1);

        <<"&"/utf8, Rest@2/binary>> ->
            escape(<<Escaped/binary, "&amp;"/utf8>>, Rest@2);

        <<"\""/utf8, Rest@3/binary>> ->
            escape(<<Escaped/binary, "&quot;"/utf8>>, Rest@3);

        <<"'"/utf8, Rest@4/binary>> ->
            escape(<<Escaped/binary, "&#39;"/utf8>>, Rest@4);

        _ ->
            case gleam@string:pop_grapheme(Content) of
                {ok, {X, Xs}} ->
                    escape(<<Escaped/binary, X/binary>>, Xs);

                {error, _} ->
                    Escaped
            end
    end.

-spec map(element(FNU), fun((FNU) -> FNW)) -> element(FNW).
map(Element, F) ->
    case Element of
        {text, Content} ->
            {text, Content};

        {element, Tag, Attrs, Children} ->
            {element,
                Tag,
                gleam@list:map(
                    Attrs,
                    fun(_capture) -> lustre@attribute:map(_capture, F) end
                ),
                gleam@list:map(
                    Children,
                    fun(_capture@1) -> map(_capture@1, F) end
                )};

        {element_ns, Tag@1, Attrs@1, Children@1, Namespace} ->
            {element_ns,
                Tag@1,
                gleam@list:map(
                    Attrs@1,
                    fun(_capture@2) -> lustre@attribute:map(_capture@2, F) end
                ),
                gleam@list:map(
                    Children@1,
                    fun(_capture@3) -> map(_capture@3, F) end
                ),
                Namespace}
    end.

-spec attrs_to_string_builder(
    gleam@string_builder:string_builder(),
    list(lustre@attribute:attribute(any()))
) -> gleam@string_builder:string_builder().
attrs_to_string_builder(Html, Attrs) ->
    {Html@2, Class@1, Style@1} = (gleam@list:fold(
        Attrs,
        {Html, <<""/utf8>>, <<""/utf8>>},
        fun(_use0, Attr) ->
            {Html@1, Class, Style} = _use0,
            case lustre@attribute:to_string_parts(Attr) of
                {ok, {<<"class"/utf8>>, Val}} when Class =:= <<""/utf8>> ->
                    {Html@1, Val, Style};

                {ok, {<<"class"/utf8>>, Val@1}} ->
                    {Html@1,
                        <<<<Class/binary, " "/utf8>>/binary, Val@1/binary>>,
                        Style};

                {ok, {<<"style"/utf8>>, Val@2}} when Style =:= <<""/utf8>> ->
                    {Html@1, Class, Val@2};

                {ok, {<<"style"/utf8>>, Val@3}} ->
                    {Html@1,
                        Class,
                        <<<<Style/binary, " "/utf8>>/binary, Val@3/binary>>};

                {ok, {Key, Val@4}} ->
                    {gleam@string_builder:append(
                            Html@1,
                            <<<<<<<<" "/utf8, Key/binary>>/binary, "=\""/utf8>>/binary,
                                    Val@4/binary>>/binary,
                                "\""/utf8>>
                        ),
                        Class,
                        Style};

                {error, _} ->
                    {Html@1, Class, Style}
            end
        end
    )),
    case {Class@1, Style@1} of
        {<<""/utf8>>, <<""/utf8>>} ->
            Html@2;

        {_, <<""/utf8>>} ->
            gleam@string_builder:append(
                Html@2,
                <<<<" class=\""/utf8, Class@1/binary>>/binary, "\""/utf8>>
            );

        {<<""/utf8>>, _} ->
            gleam@string_builder:append(
                Html@2,
                <<<<" style=\""/utf8, Style@1/binary>>/binary, "\""/utf8>>
            );

        {_, _} ->
            gleam@string_builder:append(
                Html@2,
                <<<<<<<<" class=\""/utf8, Class@1/binary>>/binary,
                            "\" style=\""/utf8>>/binary,
                        Style@1/binary>>/binary,
                    "\""/utf8>>
            )
    end.

-spec children_to_string_builder(
    gleam@string_builder:string_builder(),
    list(element(any())),
    boolean()
) -> gleam@string_builder:string_builder().
children_to_string_builder(Html, Children, Raw_text) ->
    gleam@list:fold(
        Children,
        Html,
        fun(Html@1, Child) ->
            gleam@string_builder:append_builder(
                Html@1,
                to_string_builder_helper(Child, Raw_text)
            )
        end
    ).

-spec to_string_builder_helper(element(any()), boolean()) -> gleam@string_builder:string_builder().
to_string_builder_helper(Element, Raw_text) ->
    case Element of
        {text, Content} when Raw_text ->
            gleam@string_builder:from_string(Content);

        {text, Content@1} ->
            gleam@string_builder:from_string(escape(<<""/utf8>>, Content@1));

        {element, <<"area"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"base"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"br"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"col"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"embed"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"hr"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"img"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"input"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"link"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"meta"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"param"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"source"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"track"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"wbr"/utf8>> = Tag, Attrs, _} ->
            _pipe = gleam@string_builder:from_string(<<"<"/utf8, Tag/binary>>),
            _pipe@1 = attrs_to_string_builder(_pipe, Attrs),
            gleam@string_builder:append(_pipe@1, <<">"/utf8>>);

        {element, <<"style"/utf8>> = Tag@1, Attrs@1, Children} ->
            _pipe@2 = gleam@string_builder:from_string(
                <<"<"/utf8, Tag@1/binary>>
            ),
            _pipe@3 = attrs_to_string_builder(_pipe@2, Attrs@1),
            _pipe@4 = gleam@string_builder:append(_pipe@3, <<">"/utf8>>),
            _pipe@5 = children_to_string_builder(_pipe@4, Children, true),
            gleam@string_builder:append(
                _pipe@5,
                <<<<"</"/utf8, Tag@1/binary>>/binary, ">"/utf8>>
            );

        {element, <<"script"/utf8>> = Tag@1, Attrs@1, Children} ->
            _pipe@2 = gleam@string_builder:from_string(
                <<"<"/utf8, Tag@1/binary>>
            ),
            _pipe@3 = attrs_to_string_builder(_pipe@2, Attrs@1),
            _pipe@4 = gleam@string_builder:append(_pipe@3, <<">"/utf8>>),
            _pipe@5 = children_to_string_builder(_pipe@4, Children, true),
            gleam@string_builder:append(
                _pipe@5,
                <<<<"</"/utf8, Tag@1/binary>>/binary, ">"/utf8>>
            );

        {element, Tag@2, Attrs@2, Children@1} ->
            _pipe@6 = gleam@string_builder:from_string(
                <<"<"/utf8, Tag@2/binary>>
            ),
            _pipe@7 = attrs_to_string_builder(_pipe@6, Attrs@2),
            _pipe@8 = gleam@string_builder:append(_pipe@7, <<">"/utf8>>),
            _pipe@9 = children_to_string_builder(_pipe@8, Children@1, Raw_text),
            gleam@string_builder:append(
                _pipe@9,
                <<<<"</"/utf8, Tag@2/binary>>/binary, ">"/utf8>>
            );

        {element_ns, Tag@3, Attrs@3, Children@2, Namespace} ->
            _pipe@10 = gleam@string_builder:from_string(
                <<"<"/utf8, Tag@3/binary>>
            ),
            _pipe@11 = attrs_to_string_builder(_pipe@10, Attrs@3),
            _pipe@12 = gleam@string_builder:append(
                _pipe@11,
                <<<<" xmlns=\""/utf8, Namespace/binary>>/binary, "\""/utf8>>
            ),
            _pipe@13 = gleam@string_builder:append(_pipe@12, <<">"/utf8>>),
            _pipe@14 = children_to_string_builder(
                _pipe@13,
                Children@2,
                Raw_text
            ),
            gleam@string_builder:append(
                _pipe@14,
                <<<<"</"/utf8, Tag@3/binary>>/binary, ">"/utf8>>
            )
    end.

-spec to_string(element(any())) -> binary().
to_string(Element) ->
    _pipe = to_string_builder_helper(Element, false),
    gleam@string_builder:to_string(_pipe).

-spec to_string_builder(element(any())) -> gleam@string_builder:string_builder().
to_string_builder(Element) ->
    to_string_builder_helper(Element, false).
