-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([html/2, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/1, output/2, progress/2, select/2, textarea/1, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@attribute:attribute(GSV)),
    list(lustre@element:element(GSV))
) -> lustre@element:element(GSV).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec base(list(lustre@attribute:attribute(GTB))) -> lustre@element:element(GTB).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@attribute:attribute(GTF)),
    list(lustre@element:element(GTF))
) -> lustre@element:element(GTF).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@attribute:attribute(GTL))) -> lustre@element:element(GTL).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@attribute:attribute(GTP))) -> lustre@element:element(GTP).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@attribute:attribute(GTT)), binary()) -> lustre@element:element(GTT).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [lustre@element:text(Css)]).

-spec title(list(lustre@attribute:attribute(GTX)), binary()) -> lustre@element:element(GTX).
title(Attrs, Content) ->
    lustre@element:element(
        <<"title"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec body(
    list(lustre@attribute:attribute(GUB)),
    list(lustre@element:element(GUB))
) -> lustre@element:element(GUB).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@attribute:attribute(GUH)),
    list(lustre@element:element(GUH))
) -> lustre@element:element(GUH).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@attribute:attribute(GUN)),
    list(lustre@element:element(GUN))
) -> lustre@element:element(GUN).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@attribute:attribute(GUT)),
    list(lustre@element:element(GUT))
) -> lustre@element:element(GUT).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@attribute:attribute(GUZ)),
    list(lustre@element:element(GUZ))
) -> lustre@element:element(GUZ).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@attribute:attribute(GVF)),
    list(lustre@element:element(GVF))
) -> lustre@element:element(GVF).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@attribute:attribute(GVL)),
    list(lustre@element:element(GVL))
) -> lustre@element:element(GVL).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@attribute:attribute(GVR)),
    list(lustre@element:element(GVR))
) -> lustre@element:element(GVR).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@attribute:attribute(GVX)),
    list(lustre@element:element(GVX))
) -> lustre@element:element(GVX).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@attribute:attribute(GWD)),
    list(lustre@element:element(GWD))
) -> lustre@element:element(GWD).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@attribute:attribute(GWJ)),
    list(lustre@element:element(GWJ))
) -> lustre@element:element(GWJ).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@attribute:attribute(GWP)),
    list(lustre@element:element(GWP))
) -> lustre@element:element(GWP).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@attribute:attribute(GWV)),
    list(lustre@element:element(GWV))
) -> lustre@element:element(GWV).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@attribute:attribute(GXB)),
    list(lustre@element:element(GXB))
) -> lustre@element:element(GXB).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@attribute:attribute(GXH)),
    list(lustre@element:element(GXH))
) -> lustre@element:element(GXH).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@attribute:attribute(GXN)),
    list(lustre@element:element(GXN))
) -> lustre@element:element(GXN).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@attribute:attribute(GXT)),
    list(lustre@element:element(GXT))
) -> lustre@element:element(GXT).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@attribute:attribute(GXZ)),
    list(lustre@element:element(GXZ))
) -> lustre@element:element(GXZ).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@attribute:attribute(GYF)),
    list(lustre@element:element(GYF))
) -> lustre@element:element(GYF).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@attribute:attribute(GYL)),
    list(lustre@element:element(GYL))
) -> lustre@element:element(GYL).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@attribute:attribute(GYR)),
    list(lustre@element:element(GYR))
) -> lustre@element:element(GYR).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@attribute:attribute(GYX)),
    list(lustre@element:element(GYX))
) -> lustre@element:element(GYX).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@attribute:attribute(GZD)),
    list(lustre@element:element(GZD))
) -> lustre@element:element(GZD).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@attribute:attribute(GZJ)),
    list(lustre@element:element(GZJ))
) -> lustre@element:element(GZJ).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@attribute:attribute(GZP))) -> lustre@element:element(GZP).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@attribute:attribute(GZT)),
    list(lustre@element:element(GZT))
) -> lustre@element:element(GZT).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@attribute:attribute(GZZ)),
    list(lustre@element:element(GZZ))
) -> lustre@element:element(GZZ).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@attribute:attribute(HAF)),
    list(lustre@element:element(HAF))
) -> lustre@element:element(HAF).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@attribute:attribute(HAL)),
    list(lustre@element:element(HAL))
) -> lustre@element:element(HAL).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@attribute:attribute(HAR)),
    list(lustre@element:element(HAR))
) -> lustre@element:element(HAR).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@attribute:attribute(HAX)),
    list(lustre@element:element(HAX))
) -> lustre@element:element(HAX).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@attribute:attribute(HBD)),
    list(lustre@element:element(HBD))
) -> lustre@element:element(HBD).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@attribute:attribute(HBJ)),
    list(lustre@element:element(HBJ))
) -> lustre@element:element(HBJ).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@attribute:attribute(HBP)),
    list(lustre@element:element(HBP))
) -> lustre@element:element(HBP).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@attribute:attribute(HBV)),
    list(lustre@element:element(HBV))
) -> lustre@element:element(HBV).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@attribute:attribute(HCB)),
    list(lustre@element:element(HCB))
) -> lustre@element:element(HCB).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@attribute:attribute(HCH))) -> lustre@element:element(HCH).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@attribute:attribute(HCL)),
    list(lustre@element:element(HCL))
) -> lustre@element:element(HCL).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@attribute:attribute(HCR)),
    list(lustre@element:element(HCR))
) -> lustre@element:element(HCR).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@attribute:attribute(HCX)),
    list(lustre@element:element(HCX))
) -> lustre@element:element(HCX).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@attribute:attribute(HDD)),
    list(lustre@element:element(HDD))
) -> lustre@element:element(HDD).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@attribute:attribute(HDJ)),
    list(lustre@element:element(HDJ))
) -> lustre@element:element(HDJ).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@attribute:attribute(HDP)),
    list(lustre@element:element(HDP))
) -> lustre@element:element(HDP).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@attribute:attribute(HDV)),
    list(lustre@element:element(HDV))
) -> lustre@element:element(HDV).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@attribute:attribute(HEB)),
    list(lustre@element:element(HEB))
) -> lustre@element:element(HEB).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@attribute:attribute(HEH)),
    list(lustre@element:element(HEH))
) -> lustre@element:element(HEH).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@attribute:attribute(HEN)),
    list(lustre@element:element(HEN))
) -> lustre@element:element(HEN).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@attribute:attribute(HET)),
    list(lustre@element:element(HET))
) -> lustre@element:element(HET).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@attribute:attribute(HEZ)),
    list(lustre@element:element(HEZ))
) -> lustre@element:element(HEZ).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@attribute:attribute(HFF)),
    list(lustre@element:element(HFF))
) -> lustre@element:element(HFF).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@attribute:attribute(HFL)),
    list(lustre@element:element(HFL))
) -> lustre@element:element(HFL).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@attribute:attribute(HFR)),
    list(lustre@element:element(HFR))
) -> lustre@element:element(HFR).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@attribute:attribute(HFX)),
    list(lustre@element:element(HFX))
) -> lustre@element:element(HFX).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@attribute:attribute(HGD)),
    list(lustre@element:element(HGD))
) -> lustre@element:element(HGD).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@attribute:attribute(HGJ)),
    list(lustre@element:element(HGJ))
) -> lustre@element:element(HGJ).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@attribute:attribute(HGP)),
    list(lustre@element:element(HGP))
) -> lustre@element:element(HGP).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@attribute:attribute(HGV)),
    list(lustre@element:element(HGV))
) -> lustre@element:element(HGV).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@attribute:attribute(HHB)),
    list(lustre@element:element(HHB))
) -> lustre@element:element(HHB).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@attribute:attribute(HHH)),
    list(lustre@element:element(HHH))
) -> lustre@element:element(HHH).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@attribute:attribute(HHN))) -> lustre@element:element(HHN).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@attribute:attribute(HHR))) -> lustre@element:element(HHR).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@attribute:attribute(HHV)),
    list(lustre@element:element(HHV))
) -> lustre@element:element(HHV).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@attribute:attribute(HIB))) -> lustre@element:element(HIB).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@attribute:attribute(HIF)),
    list(lustre@element:element(HIF))
) -> lustre@element:element(HIF).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@attribute:attribute(HIL))) -> lustre@element:element(HIL).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@attribute:attribute(HIP)),
    list(lustre@element:element(HIP))
) -> lustre@element:element(HIP).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@attribute:attribute(HIV))) -> lustre@element:element(HIV).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@attribute:attribute(HIZ))) -> lustre@element:element(HIZ).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@attribute:attribute(HJD))) -> lustre@element:element(HJD).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@attribute:attribute(HJH)),
    list(lustre@element:element(HJH))
) -> lustre@element:element(HJH).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@attribute:attribute(HJN))) -> lustre@element:element(HJN).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@attribute:attribute(HJR))) -> lustre@element:element(HJR).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@attribute:attribute(HJV)),
    list(lustre@element:element(HJV))
) -> lustre@element:element(HJV).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@attribute:attribute(HKB)),
    list(lustre@element:element(HKB))
) -> lustre@element:element(HKB).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@attribute:attribute(HKH))) -> lustre@element:element(HKH).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@attribute:attribute(HKL)),
    list(lustre@element:element(HKL))
) -> lustre@element:element(HKL).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@attribute:attribute(HKR)), binary()) -> lustre@element:element(HKR).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [lustre@element:text(Js)]).

-spec del(
    list(lustre@attribute:attribute(HKV)),
    list(lustre@element:element(HKV))
) -> lustre@element:element(HKV).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@attribute:attribute(HLB)),
    list(lustre@element:element(HLB))
) -> lustre@element:element(HLB).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@attribute:attribute(HLH)),
    list(lustre@element:element(HLH))
) -> lustre@element:element(HLH).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@attribute:attribute(HLN))) -> lustre@element:element(HLN).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@attribute:attribute(HLR)),
    list(lustre@element:element(HLR))
) -> lustre@element:element(HLR).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@attribute:attribute(HLX)),
    list(lustre@element:element(HLX))
) -> lustre@element:element(HLX).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@attribute:attribute(HMD)),
    list(lustre@element:element(HMD))
) -> lustre@element:element(HMD).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@attribute:attribute(HMJ)),
    list(lustre@element:element(HMJ))
) -> lustre@element:element(HMJ).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@attribute:attribute(HMP)),
    list(lustre@element:element(HMP))
) -> lustre@element:element(HMP).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@attribute:attribute(HMV)),
    list(lustre@element:element(HMV))
) -> lustre@element:element(HMV).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@attribute:attribute(HNB)),
    list(lustre@element:element(HNB))
) -> lustre@element:element(HNB).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@attribute:attribute(HNH)),
    list(lustre@element:element(HNH))
) -> lustre@element:element(HNH).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@attribute:attribute(HNN)),
    list(lustre@element:element(HNN))
) -> lustre@element:element(HNN).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@attribute:attribute(HNT)),
    list(lustre@element:element(HNT))
) -> lustre@element:element(HNT).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@attribute:attribute(HNZ)),
    list(lustre@element:element(HNZ))
) -> lustre@element:element(HNZ).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@attribute:attribute(HOF)),
    list(lustre@element:element(HOF))
) -> lustre@element:element(HOF).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@attribute:attribute(HOL))) -> lustre@element:element(HOL).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@attribute:attribute(HOP)),
    list(lustre@element:element(HOP))
) -> lustre@element:element(HOP).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@attribute:attribute(HOV)),
    list(lustre@element:element(HOV))
) -> lustre@element:element(HOV).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@attribute:attribute(HPB)),
    list(lustre@element:element(HPB))
) -> lustre@element:element(HPB).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@attribute:attribute(HPH)),
    list(lustre@element:element(HPH))
) -> lustre@element:element(HPH).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@attribute:attribute(HPN))) -> lustre@element:element(HPN).
option(Attrs) ->
    lustre@element:element(<<"option"/utf8>>, Attrs, []).

-spec output(
    list(lustre@attribute:attribute(HPR)),
    list(lustre@element:element(HPR))
) -> lustre@element:element(HPR).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@attribute:attribute(HPX)),
    list(lustre@element:element(HPX))
) -> lustre@element:element(HPX).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@attribute:attribute(HQD)),
    list(lustre@element:element(HQD))
) -> lustre@element:element(HQD).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@attribute:attribute(HQJ))) -> lustre@element:element(HQJ).
textarea(Attrs) ->
    lustre@element:element(<<"textarea"/utf8>>, Attrs, []).

-spec details(
    list(lustre@attribute:attribute(HQN)),
    list(lustre@element:element(HQN))
) -> lustre@element:element(HQN).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@attribute:attribute(HQT)),
    list(lustre@element:element(HQT))
) -> lustre@element:element(HQT).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@attribute:attribute(HQZ)),
    list(lustre@element:element(HQZ))
) -> lustre@element:element(HQZ).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@attribute:attribute(HRF))) -> lustre@element:element(HRF).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@attribute:attribute(HRJ)),
    list(lustre@element:element(HRJ))
) -> lustre@element:element(HRJ).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
