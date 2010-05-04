/*

 JSParsec - A parser combinator library for JavaScript

 Version: 0.0.5

 http://code.google.com/p/jsparsec/

 Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 Dual licensed under the MIT and GPL licenses.


 The initial implementation of some combinators and the memoization is derived from:
 http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html

 Most functions should behave like their counterparts in Parsec:
 http://www.haskell.org/haskellwiki/Parsec

*/
(function(){function Y(a){function b(){var c=B(arguments);return c.length>=(a._length===undefined?a.length:a._length)?a.apply(null,c):function(){return b.apply(null,c.concat(B(arguments)))}}return b}function Ua(a){return function(){return a}}function ia(a){return wc.call(a)=="[object Array]"}function Va(a){return a!==w}function B(a,b){return xc.call(a,b||0)}function Gb(a,b,c){for(var d=0,f=c.length;d<f;++d)b=a(b,c[d]);return b}function ta(a,b,c){for(var d=c.length-1;d>-1;--d)b=a(c[d],b);return b}
function y(a,b){for(var c=[],d=0,f=b.length;d<f;++d)c[d]=a(b[d],d);return c}function Hb(a,b){for(var c=[],d=0,f=a.length;d<f;++d)b(a[d])&&c.push(a[d]);return c}function Wa(a,b){var c=a.length;if(!c)return-1;for(var d=0;d<c;d++)if(a[d]===b)return d;return-1}function Ib(a,b){var c=a.length;if(!c)return-1;for(c=c-1;c>-1;--c)if(a[c]===b)return c;return-1}function Xa(a,b){for(var c=[],d=0,f=Math.min(a.length,b.length);d<f;++d)c[d]=[a[d],b[d]];return c}function Ya(a){var b=typeof a;if(b=="object")return a.sort();
if(b=="string")return B(a).sort().join("")}function Ja(a,b){b=b===w?[]:b;var c=a[0],d=B(a,1);return!a.length?[]:Ka(c,b)?Ja(d,b):R(c,Ja(d,R(c,b)))}function Jb(a,b,c){if(c.Nothing)return a;if(c.Just)return b(c[0])}function Kb(a,b){return a===b?ua.EQ:a<=b?ua.LT:ua.GT}function z(a,b){for(var c in b)a[c]=b[c];return a}function Lb(a,b){return function(){return a(b.apply(null,arguments))}}function Mb(a,b){return function(c){var d=B(arguments,1);d.unshift(b(c));return a.apply(null,d)}}function Za(a){return function(b,
c){return a(c,b)}}function R(a,b){if(typeof a=="string"&&typeof b=="string")return a+b;return[a].concat(b)}function $a(a,b){if(typeof a=="string"&&typeof b=="string")return a+b;return a+b.join("")}function ab(a,b){for(var c=[],d=0;d<a;++d)c[d]=b;return c}function Nb(a){return-a}function La(a){return!a.length}function Ka(a,b){return(b.indexOf?b.indexOf(a):Wa(b,a))!=-1}function bb(a){return/^\s$/.test(a)}function Ob(a){return a.toUpperCase()==a}function Pb(a){return a.toLowerCase()==a}function Qb(a){return/^\w$/.test(a)}
function cb(a){return/^\w$/.test(a)&&/^\D$/.test(a)}function Rb(a){return/^\d$/.test(a)}function db(a){return/^[0-9A-Fa-f]$/.test(a)}function Sb(a){return/^[0-7]$/.test(a)}function eb(a){if(!db(a))throw"Data.Char.digitToInt: not a digit "+a;return parseInt(a,16)}function yc(a,b){return{indexOf:function(c){return c>=a&&c<=b?true:-1},toString:function(){return"range("+a+", "+b+")"}}}function zc(){var a,b;y(function(c){b=c.split(".");a=window[b[0]]=window[b[0]]||{};y(function(d){a=a[d]=a[d]||{}},b.slice(1))},
arguments);return a}function Tb(){}function Ac(a,b){if(!/^[0-9]$/.test(b))return b;if(typeof a!="object")return b;var c=0;for(var d in a){if(a.hasOwnProperty(d)&&c==b)return d;c++}return-1}function Ma(){}function Ub(a){return function(){var b=[],c=this._recordset;if(ia(c)){for(var d=0;d in this;d++)b.push(a?c[d].name||c[d]:this[d]);b=b.join(" ")}else{for(d in c){var f=a?c[d].name||c[d]:this[d];if(!a&&f instanceof Function)f=f.constructor!=Function?f.constructor.name:"Function("+f.name+")";b.push(d+
" :: "+f)}c=ab(this._dataConstructor.length+2," ").join("");b="{"+b.join("\n"+c+",")+"\n"+c+"}"}return this._dataConstructor+" "+b}}function ja(a,b){function c(j,i){function r(L,C){var Na=L instanceof Tb,S=Na?C:B(arguments),I=new a,F=0;I.constructor=a;I._recordset=s&&i[0]||i;I._dataConstructor=j;I.update=function(va){var wa={};for(var Z in i[0]){wa[Z]=this[Z];if(Z in va)wa[Z]=va[Z]}return r(xa,wa)};I[j]=true;if(S!==w)for(var M in S)if(S.hasOwnProperty(M)&&M!=j){if(Na&&i&&s)if(!(M in i[0]))throw"The accessor '"+
M+"' is not defined for the data constructor '"+j+"'";var ka=Ac(i[0],M),la=S[F]!==undefined?S[F]:S[ka],$=s?i[0][ka]:i[F];if($.name&&!($==la.constructor||la instanceof $))throw"Type error: expecting '"+$.name+"' instead of '"+la.constructor.name+"' in the argument '"+(ka||F)+"' of the data constructor '"+j+"' of type '"+a.name+"'";I[ka]=I[F]=I[M]=S[M];F++}else if(M==j)throw"Accessor has the same name as the data constructor: '"+j+"'";return I}var s=i&&typeof i[0]=="object";return r}if(a.constructors)throw"Type constructor has been already defined: '"+
a.name+"'";a.constructors=b;a.prototype=new Ma;for(var d=0,f=b.length;d<f;++d){var g=typeof b[d]!="object",h=g?b[d]:b[d][0];if(h in{})throw"The name of the data constructor can't be a property of Object.prototype as well!";a[h]=g?c(h)():c(h,B(b[d],1));if(!g)a[h]._length=B(b[d],1).length}}function V(){}function ua(){}function ma(){}function T(a){return["l",a]}function aa(a){return["r",a]}function Vb(a){return["x",a]}function Bc(a){if(a)if(!a._String){var b=na[a];if(a._Op&&!b)return"l";return b&&(b.fixity[0]||
"l")}}function Cc(a){if(a)if(!a._String){var b=na[a];if(a._Op&&!b)return 9;return b&&(Va(b.fixity[1])?b.fixity[1]:9)}}function fb(a,b,c){var d;if(a[b]._Op){delete a[b]._Op;d=a[b]}else d=na[a[b]].func;d=d(a[b-1],a[b+1]);a.splice(b-1,3,d);return ba(a,c)}function Dc(a){a._Array=true;return a}function Ec(a){a=new String(a);a._String=true;return a}function Fc(a){a._Op=true;return a}function ba(a,b){a=y(function(i){if(i&&i._Array){delete i._Array;return i}return ia(i)?ba(i,b):i&&i.CallStream?i.resolve():
i},a);if(b)a=y(function(i){return i instanceof gb?b:i},a);for(var c=[],d=[],f=0,g=a.length;f<g;++f){var h=a[f],j=false;if(na[h])j=true;if(h&&h._String){j=false;h=h.toString()}if(h&&h._Op)j=true;if(!j&&f!=g-1)c.push(h);else{f==g-1&&c.push(h);c=c.length>1?c[0].apply(null,c.slice(1)):c[0];d.push(c);f!=g-1&&d.push(h);c=[]}}a=d;d=y(Bc,a);g=y(Cc,a);h=Hb(g,Va).sort().pop();f=Wa(g,h);g=Ib(g,h);return d[f]=="l"?fb(a,f,b):d[g]=="r"?fb(a,g,b):d[f]=="x"?fb(a,f,b):a[0]}function gb(){}function n(){function a(g,
h,j){return d(g,h,j)}function b(g,h,j){if(g instanceof ya)return(f?d:b.resolve())(g,h,j);c.push(ba(arguments,a));return b}var c=[],d,f;c.push(ba(arguments,a));b.resolve=function(){if(f)return d;d=k.apply(null,c);c=null;f=true;return d};b.CallStream=true;return b}function ya(a,b){this.input=a;this.index=b||0;this.length=a.length-this.index;this.cache={};return this}function Wb(a){return new ya(a)}function A(a,b,c){return{ast:a,success:b===w?true:b,expecting:c}}function oa(a){return A(w,false,a)}function Oa(a){return function(b,
c,d){return d(A(null,false,{unexpected:c[a]}))}}function Xb(a){return function(b,c,d){return d(A(w,false,a))}}function Yb(a,b,c){return c(A(w,false))}function hb(a){return typeof a=="string"?N(a):ia(a)?ba(a):a}function Gc(a){for(;a&&a.func;)a=a.func.apply(null,a.args||[])}function Hc(a,b){b=b||0;b++;if(a&&a.func){a=a.func.apply(null,a.args||[]);b%500==0?setTimeout(function(){trampoline2(a,b)},1):trampoline2(a,b)}else b=0}function Ic(a,b,c,d,f){var g=b instanceof ya?b:Wb(b);(f?Hc:Gc)({func:a,args:[g,
{},function(h){h.state=g;delete h.index;delete h.length;if(h.success){delete h.error;delete h.expecting}else{h.error=ib(h.expecting,h.state);d&&d(h.error)}c(h)}]})}function ib(a,b,c,d){c=c===undefined?b.index:c;if(typeof a=="string"){var f=b.input.split("\n"),g=f.length,h=b.input.substr(c).split("\n").length;g=g-h+1;f=c-f.splice(0,g-1).join("\n").length;b=d||b.input.substr(c,a.length).substr(0,6);return'Unexpected "'+(b.length?b:"end of file")+(d?"":'", expecting "'+a)+'" at line '+g+" char "+f}if(ia(a)){a=
y(function(j){return typeof j=="object"?j.expecting:j},a);return ib(a.join('" or "'),b)}else if(typeof a=="object")return ib(a.expecting,b,a.at,a.unexpected)}function o(){}function jb(a,b){return function(c,d,f){return{func:a,args:[c,d,function(g){return f(b(g))}]}}}function q(a,b){if(a=="scope")throw"Can't use 'scope' as an identifier!";return function(c,d,f){return{func:b,args:[c,d,function(g){if(g.success)d[a]=g.ast;return f(g)}]}}}function t(a,b){b&&B(arguments);return function(c,d,f){return{func:function(){var g;
if(typeof a=="string"){if(!(a in d))throw'Not in scope: "'+a+'"';g=d[a]}else g=a(d);return f(A(g))}}}}function kb(a){var b=B(arguments,1);return function(c){return a.apply(null,y(function(d){return c[d]},b))}}function Zb(a,b,c){return c(A(a.index))}function $b(a){return function(b,c,d){b.scrollTo(c[a]);return d(Jc)}}function ac(a){return function(b,c,d){return d(A(a))}}function za(a,b){return pa(function(c){return c[0](c[1])},lb(a,b))}function bc(a,b){a=Y(a);return function(c,d,f){return{func:b,args:[c,
d,function(g){if(!g.success)return f(g);g=z({},g);g.ast=a(g.ast);return f(g)}]}}}function cc(a,b){return k(q("a",a),b,t("a"))}function v(a,b){function c(d,f,g){return{func:a,args:[d,f,function(h){function j(r){var s=r.expecting;if(s)if(ia(s))i=i.concat(s);else i.push(s);if(r.success)delete r.expecting;else r.expecting=i}var i=[];j(h);return h.ast!==undefined?{func:g,args:[h]}:{func:b,args:[d,f,function(r){j(r);return g(r)}]}}]}}c.constructor=o;return c}function Kc(){var a=y(hb,arguments);return function(b,
c,d){for(var f=1,g=a.length,h=a[0];f<g;++f)h=v(h,a[f]);return h(b,c,d)}}function lb(a){return function(b,c,d){function f(i){return function(r,s,L){return{func:i,args:[r,s,function(C){g++;if(!C.success)return L(C);C.ast!==w&&h.push(C.ast);return g<j?f(a[g])(r,s,L):L(C)}]}}}var g=0,h=[],j=a.length;return{func:f(a[g]),args:[b,c,function(i){i=z({},i);i.ast=h;i.success&&delete i.expecting;return d(i)}]}}}function dc(a){return function(b){return function(c,d,f){function g(i){return function(r,s,L){return{func:i,
args:[r,s,function(C){if(!C.success)return L(C);h=true;C.ast!==w&&j.push(C.ast);return g(i)(r,s,L)}]}}}var h=false,j=[];return{func:g(b),args:[c,d,function(i){i=z({},i);i.success=!a||h&&a;i.ast=j;i.success&&delete i.expecting;return f(i)}]}}}}function mb(a){return function(b){var c=ec++,d=function(f,g,h){g=f.index;var j=f.getCached(c);if(j!==w)return h(j);j=a(b,f,g);f.putCached(c,g,j);return h(j)};d.constructor=o;return d}}function Pa(a){return function(b,c){var d=ec++,f=function(g,h,j){var i=g.index,
r=g.getCached(d);if(r!==w)return j(r);return{func:b,args:[g,h,function(s){s=a(c,s,g,i);g.putCached(d,i,s);return j(s)}]}};f.constructor=o;return f}}function fc(a){function b(c,d){return k(q("x",c),q("xs",d),t(kb(R,"x","xs")))}return ta(b,m([]),a)}function nb(a){return ta(v,ob,a)}function Qa(a,b){return v(b,m(a))}function Lc(a){return Qa(V.Nothing,pb(V.Just,a))}function Mc(a){return v(k(a,m(null)),m(null))}function Ra(a){return k(a,W(a))}function qb(a,b){return v(Sa(a,b),m([]))}function Sa(a,b){return k(q("x",
a),q("xs",ca(k(b,a))),O(R,"x","xs"))}function gc(a,b){return k(q("x",a),v(k(b,q("xs",function(c,d,f){return hc(a,b)(c,d,f)}),t(function(c){return R(c.scope.x,c.xs)})),t(function(c){return[c.x]})))}function hc(a,b){return v(gc(a,b),m([]))}function Nc(a,b){return Aa(k(q("x",a),b,t("x")))}function Oc(a,b){return ca(k(q("x",a),b,t("x")))}function Pc(a,b){return a<=0?m([]):fc(ab(a,b))}function Qc(a,b,c){return v(ic(a,b),m(c))}function Rc(a,b,c){return v(jc(a,b),m(c))}function jc(a,b){function c(d){var f=
k(q("f",b),q("y",a),function(g,h,j){return c(h.f(d,h.y))(g,h,j)});return v(f,m(d))}return k(q("x",a),function(d,f,g){return c(f.x)(d,f,g)})}function ic(a,b){function c(f){var g=k(q("f",b),q("y",d),function(h,j,i){return i(A(j.f(f,j.y)))});return v(g,m(f))}var d=k(q("x",a),function(f,g,h){return c(g.x)(f,g,h)});return d}function Sc(a,b,c){b=a.at(0);if(b.length){a.scroll(1);return c(A(b))}return c(oa("anyToken"))}function Tc(a,b,c){return c(A(w,!a.length,a.length?"end of input":w))}function rb(a){return G(v(k(q("c",
G(a)),Oa("c")),m(null)))}function Uc(a,b){function c(f,g,h){return d(f,g,h)}var d=v(k(b,m([])),k(q("x",a),q("xs",c),O(R,"x","xs")));return d}function Vc(a){return k(q("state",Zb),q("x",a),$b("state"),t("x"))}function Ba(){}function sb(){}function tb(a){function b(e){return c(N(e))}function c(e){return k(q("x",e),kc,t("x"))}function d(e,l){return lc(e,l)}function f(e){return mc(e)}function g(e){return ub(e)}function h(e){return da(b("("),b(")"),e)}function j(e){return da(b("{"),b("}"),e)}function i(e){return da(b("<"),
b(">"),e)}function r(e){return da(b("["),b("]"),e)}function s(e){return qb(e,vb)}function L(e){return qb(e,wb)}function C(e){return Sa(e,vb)}function Na(e){return Sa(e,wb)}function S(e){return k(p(e[0]),m(e[1]))}function I(e){return G(k(N(e[0]),m(e[1])))}function F(e,l){return n("digits","<-",Aa,l)(t,function(D){return Gb(function(E,Ca){return e*E+nc(eb(Ca))},0,D.digits)}).resolve()}function M(e,l){return ea(e,l)}function ka(e,l){return(l+oc(eb(e)))/10}function la(e){return e<0?1/la(-e):Da(Math.pow(10,
e))}function $(e){return[n("fract","<-",Wc)("expo","<-",Qa,1,pc)(t,function(l){return Da(e+l.fract)*l.expo}),"<|>",n("expo","<-",pc)(t,function(l){return Da(e)*l.expo})].resolve()}function va(e){return n("f","<-",$,e)(O,ma.Right,"f").resolve()}function wa(e){return[c,"$",G,"$",n(N(e))(rb,a.opLetter,"<?>","end of "+e)].resolve()}function Z(e){return qc(Ya(a.reservedOpNames),e)}function Xc(e){function l(E){return!E.length?m(null):k(Ea(D(E[0]),""+e),l(B(E,1)))}function D(E){return cb(E)?v(p(E.toLowerCase()),
p(E.toUpperCase())):p(E)}return a.caseSensitive?N(e):k(l(e),m(e))}function Yc(e){return[c,"$",G,"$",n(Xc(e))(rb,a.identLetter,"<?>","end of "+e)].resolve()}function qc(e,l){function D(E){if(!E.length)return false;var Ca=Kb(E[0],l);return Ca.LT?D(B(E,1)):Ca.EQ?true:Ca.GT?false:null}return D(e)}function Zc(e){e=a.caseSensitive?e:e.toLowerCase();return qc($c,e)}if(!a.LanguageDef)throw"Type error: unexpected '"+a.constructor.name+"', expecting 'GenLanguageDef.LanguageDef'";var qa=Ra(x(bb)),Ta=n(G(N(a.commentLine)))(W,
x(function(e){return e!="\n"}))(m,null).resolve(),u=Ja(B(a.commentEnd+a.commentStart)),lc=[k(G(N(a.commentEnd)),m(null)),"<|>",k(Ra(xb(u)),d),"<|>",k(P(u),d),"<?>","end of comment"].resolve(),mc=[k(G(N(a.commentEnd)),m(null)),"<|>",k(g,f),"<|>",k(Ra(xb(u)),f),"<|>",k(P(u),f),"<?>","end of comment"].resolve();u=a.nestedComments?mc:lc;var ub=k(G(N(a.commentStart)),u);u=La(a.commentLine);var Q=La(a.commentStart),kc=(u&&Q?[W,[qa,"<?>",""]]:u?[W,[qa,"<|>",ub,"<?>",""]]:Q?[W,[qa,"<|>",Ta,"<?>",""]]:[W,
[qa,"<|>",Ta,"<|>",ub,"<?>",""]]).resolve(),wb=b(";"),vb=b(",");qa=b(".");Ta=b(":");u=Xa("abfnrtv\\\"'","a\u0008\u000c\n\r\t\u000b\\\"'");Q=Xa(["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","SUB","ESC","DEL"]+["BS","HT","LF","VT","FF","CR","SO","SI","EM","FS","GS","RS","US","SP"],["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","SUB","ESC","DEL"]+["BS","HT","LF","VT","FF","CR","SO","SI","EM",
"FS","GS","RS","US","SP"]);u=nb(y(S,u));var U=nb(y(I,Q));Q=x(function(e){return e!='"'&&e!="\\"&&e>"\u0016"});var fa=p("&"),Fa=n(Aa,yb)(p("\\"),"<?>","end of string gap").resolve(),Ga=n("code","<-",M,"<|>",k(p("o"),F(8,zb)),"<|>",k(p("x"),F(16,Ab)))(t(function(e){return toEnum(Da(e.code))})).resolve(),X=n(p("^"))("code","<-",rc)(t(function(e){return toEnum(fromEnum(e.code)-fromEnum("A"))})).resolve();U=[u,"<|>",Ga,"<|>",U,"<|>",X,"<?>","escape code"].resolve();u=k(p("\\"),U);u=[x(function(e){return e!=
"'"&&e!="\\"&&e>"\u0016"}),"<|>",u,"<?>","literal character"].resolve();u=[c,[da,p("'"),[p("'"),"<?>","end of character"],u],"<?>","character"].resolve();fa=n(p("\\"))(n(Fa)(m,V.Nothing),"<|>",n(fa)(m,V.Nothing),"<|>",n("esc","<-",U)(O,V.Just,"esc")).resolve();Q=[n("c","<-",Q)(O,V.Just,"c"),"<|>",fa,"<?>","string character"].resolve();Q=c([n("str","<-",da,p('"'),[p('"'),"<?>","end of string"],[ca,Q])(t,function(e){return ta(Y(Jb)(ra,Y(R)),"",e.str)}),"<?>","literal string"].resolve());var ea=F(10,
Bb);fa=n(P,"xX")(F,16,Ab).resolve();Fa=n(P,"oO")(F,8,zb).resolve();var Wc=[n(p("."))("digits","<-",Aa,Bb,"<?>","fraction")(t,function(e){return ta(ka,0,e.digits)}),"<?>","fraction"].resolve();U=[[p("-"),">>",m,Nb],"<|>",[p("+"),">>",m,ra],"<|>",m,ra].resolve();var pc=[n(P,"eE")("f","<-",U)("e","<-",ea,"<?>","exponent")(O,la,"f","e"),"<?>","exponent"].resolve();Ga=n("n","<-",ea)(function(e,l,D){return $(l.n)(e,l,D)}).resolve();X=n("n","<-",ea)(function(e,l,D){return Qa(ma.Left(l.n),va(l.n))(e,l,D)}).resolve();
var sa=[n("n","<-",fa,"<|>",Fa)(O,ma.Left,"n"),"<|>",X,"<|>",va(0),"<|>",m,ma.Left(0)].resolve(),Ha=[k(p("0"),sa),"<|>",X].resolve();sa=[[n(p,"0")(fa,"<|>",Fa,"<|>",ea,"<|>",m,0),"<?>",""].resolve(),"<|>",ea].resolve();X=n("f","<-",c,U)("n","<-",sa)(t,function(e){return e.f(e.n)});U=[c,Ha,"<?>","number"].resolve();Ga=[c,Ga,"<?>","float"].resolve();X=[c,X,"<?>","integer"].resolve();sa=[c,sa,"<?>","natural"].resolve();Ha=[n("c","<-",a.opStart)("cs","<-",ca,a.opLetter)(O,$a,"c","cs"),"<?>","operator"].resolve();
Ha=[c,"$",G,"$",n("name","<-",Ha)(function(e,l,D){return(Z(l.name)?Oa("reserved operator "+l.name):m(l.name))(e,l,D)})].resolve();var Cb=[n("c","<-",a.identStart)("cs","<-",ca,a.identLetter)(O,$a,"c","cs"),"<?>","identifier"].resolve();Cb=[c,"$",G,"$",n("name","<-",Cb)(function(e,l,D){return(Zc(l.name)?Oa("reserved word "+l.name):m(l.name))(e,l,D)})].resolve();var sc=Ya(a.reservedNames),$c=a.caseSensitive?sc:y(function(e){return e.toLowerCase()},sc);return sb.TokenParser(xa,{identifier:Cb,reserved:Yc,
operator:Ha,reservedOp:wa,charLiteral:u,stringLiteral:Q,natural:sa,integer:X,float_:Ga,naturalOrFloat:U,decimal:ea,hexadecimal:fa,octal:Fa,symbol:b,lexeme:c,whiteSpace:kc,parens:h,braces:j,angles:i,brackets:r,squares:r,semi:wb,comma:vb,colon:Ta,dot:qa,semiSep:L,semiSep1:Na,commaSep:s,commaSep1:C})}var w,wc={}.toString,xc=[].slice,ra=function(a){return a},J=Y(function(a,b){return a(b)}),nc=parseInt,Da=ra,oc=ra,ga=window.JSParsec={};z(ga,{curry:Y,const_:Ua,"const":Ua,isArray:ia,isDefined:Va,slice:B,
foldl:Gb,foldr:ta,map:y,filter:Hb,indexOf:Wa,lastIndexOf:Ib,zip:Xa,sort:Ya,nub:Ja,maybe:Jb,compare:Kb,compose:Lb,compose1:Mb,call:J,id:ra,flip:Za,cons:R,consJoin:$a,replicate:ab,negate:Nb,null_:La,"null":La,elem:Ka,isSpace:bb,isUpper:Ob,isLower:Pb,isAlpha:cb,isAlphaNum:Qb,isDigit:Rb,isHexDigit:db,isOctDigit:Sb,digitToInt:eb,range:yc,extend:z,namespace:zc,toInteger:nc,fromInteger:Da,fromIntegral:oc});var xa=new Tb;Ma.prototype.toString=Ub();Ma.prototype.dataConstructorToString=Ub(true);ja(V,[["Just",
"a"],"Nothing"]);ja(ua,["LT","EQ","GT"]);ja(ma,[["Left","a"],["Right","b"]]);var na={$:{func:J,fixity:aa(0)},".":{func:Mb,fixity:aa(9)},":":{func:R,fixity:aa(5)}};Array.prototype.resolve=function(){return ba(this)};var H=new gb;z(ga,{data:ja,ADT:Ma,Maybe:V,Ordering:ua,Either:ma,operators:na,infix:Vb,infixl:T,infixr:aa,arr:Dc,op:Fc,str:Ec,resolve:ba,recurse:H,Recurse:gb,cs:n});ya.prototype={memoize:false,scrollTo:function(a){this.index=a;this.length=this.input.length-a;return this},scroll:function(a){this.index+=
a;this.length-=a;return this},at:function(a){return this.input.charAt(this.index+a)},substring:function(a,b){return this.input.substring(a+this.index,(b||this.length)+this.index)},substr:function(a,b){return this.input.substring(a+this.index,b||this.length)},toString:function(){var a=this.substring(0);return"PS at "+this.index+" "+(a.length?'"'+a+'"':"Empty")},getCached:function(a){if(this.memoize)if(a=this.cache[a])if(a=a[this.index]){this.index=a.index;this.length=a.length;return a}},putCached:function(a,
b,c){if(!this.memoize)return false;c.index=this.index;c.length=this.length;var d=this.cache[a];d||(d=this.cache[a]={});d[b]=c}};var Jc=A(w),ob=Yb;H=ob;var ec=0,Db=function(a,b){function c(d,f,g){return{func:a,args:[d,f,function(h){return h.success?b(d,f,g):g(h)}]}}c.constructor=o;return c},k=function(){function a(c,d,f){var g={},h=1,j=b.length,i=b[0];for(g.scope=d;h<j;++h)i=Db(i,b[h]);return i(c,g,f)}var b=y(hb,arguments);a.constructor=o;return a},O=Lb(t,kb),m=ac,tc=m,pa=bc,pb=pa,Ia=pb,ha=function(a,
b,c){return za(pa(a,b),c)},K=Db,Eb=v,ca=dc(false),Aa=dc(true),G=Pa(function(a,b,c,d){if(b.success)return b;c.scrollTo(d);return oa(b.expecting)}),W=function(a){return Pa(function(b,c){c=z({},c);c.ast=w;return c})(ca(a),null)},p=mb(function(a,b){if(b.length>0&&b.at(0)==a){b.scroll(1);return A(a)}return oa(a)}),x=mb(function(a,b){var c=b.at(0);if(b.length>0&&a(c)){b.scroll(1);return A(c)}return oa(c)}),N=function(a){return Pa(function(b,c,d,f){c.ast=c.ast.join("");c=z({},c);if(c.success)delete c.expecting;
else c.expecting={at:f,expecting:a};if(!c.ast.length)c.ast=w;return c})(lb(y(p,a)),null)},Ea=Pa(function(a,b,c,d){if(!b.success){b=z({},b);b.expecting={at:d,expecting:a}}return b}),Fb=mb(function(a,b){var c;if(typeof a=="string")if(b.substring(0,a.length)==a){b.scroll(a.length);c=A(a)}else c=oa(a);else if(a.exec){var d=new RegExp("^"+a.source);c=b.substring(0);d=(d=(d=d.exec(c))&&d[0])&&d.length;c=c.substr(0,d);if(d){b.scroll(d);c=A(c)}else c=oa(a.source)}return c});z(na,{"<-":{func:q,fixity:aa(-1)},
">>=":{func:jb,fixity:T(1)},"=<<":{func:Za(jb),fixity:aa(1)},">>":{func:K,fixity:T(1)},"*>":{func:K,fixity:T(4)},"<*":{func:cc,fixity:T(4)},"<$>":{func:pa,fixity:T(4)},"<*>":{func:za,fixity:T(4)},"<**>":{func:Y(ha)(Za(J)),fixity:T(4)},"<$":{func:function(a,b){return cc(tc(value),b)},fixity:T(4)},"<|>":{func:v,fixity:aa(1)},"<?>":{func:Ea,fixity:Vb(0)}});z(ga,{sequence:fc,run:Ic,Parser:o,ParseState:ya,ps:Wb,toParser:hb,unexpected:Oa,parsecMap:bc,fmap:pa,liftM:pb,liftA:Ia,liftA2:ha,liftA3:function(a,
b,c,d){return za(za(pa(a,b),c),d)},ap:za,parserBind:jb,parserReturn:ac,return_:m,pure:tc,parserFail:Xb,fail:Xb,parserZero:Yb,mzero:ob,empty:H,parserPlus:v,parserPlusN:Kc,mplus:Eb,do_:k,do2:Db,bind:q,ret:t,withBound:kb,returnCall:O,getParserState:Zb,setParserState:$b,tokens:lb,many:ca,many1:Aa,string:N,char_:p,satisfy:x,label:Ea,try_:G,skipMany:W,match:Fb});var P=function(a){return Ea(x(function(b){return Ka(b,a)}),"oneOf("+a+")")},xb=function(a){return Ea(x(function(b){return!Ka(b,a)}),"noneOf("+
a+")")},yb=[x,bb,"<?>","space"].resolve();Ia=[W,yb,"<?>","white space"].resolve();ha=[p,"\n","<?>","new-line"].resolve();K=[p,"\t","<?>","tab"].resolve();var rc=[x,Ob,"<?>","uppercase letter"].resolve();Eb=[x,Pb,"<?>","lowercase letter"].resolve();J=[x,Qb,"<?>","letter or digit"].resolve();H=[x,cb,"<?>","letter"].resolve();var Bb=[x,Rb,"<?>","digit"].resolve(),Ab=[x,db,"<?>","hexadecimal digit"].resolve(),zb=[x,Sb,"<?>","octal digit"].resolve();Fb=[x,Ua(true)].resolve();z(ga,{oneOf:P,noneOf:xb,space:yb,
spaces:Ia,newline:ha,tab:K,upper:rc,lower:Eb,alphaNum:J,letter:H,digit:Bb,hexDigit:Ab,octDigit:zb,anyChar:Fb});var da=Y(function(a,b,c){return k(a,q("x",c),b,t("x"))});z(ga,{choice:nb,count:Pc,between:da,option:Qa,optionMaybe:Lc,optional:Mc,skipMany1:Ra,sepBy:qb,sepBy1:Sa,endBy:Oc,endBy1:Nc,sepEndBy:hc,sepEndBy1:gc,chainl:Rc,chainl1:jc,chainr:Qc,chainr1:ic,eof:Tc,notFollowedBy:rb,manyTill:Uc,lookAhead:Vc,anyToken:Sc});ja(Ba,[["LanguageDef",{commentStart:String,commentEnd:String,commentLine:String,
nestedComments:Boolean,identStart:o,identLetter:o,opStart:o,opLetter:o,reservedNames:Array,reservedOpNames:Array,caseSensitive:Boolean}]]);ja(sb,[["TokenParser",{identifier:o,reserved:Function,operator:o,reservedOp:Function,charLiteral:o,stringLiteral:o,natural:o,integer:o,float_:o,naturalOrFloat:o,decimal:o,hexadecimal:o,octal:o,symbol:Function,lexeme:Function,whiteSpace:o,parens:Function,braces:Function,angles:Function,brackets:Function,squares:Function,semi:o,comma:o,colon:o,dot:o,semiSep:Function,
semiSep1:Function,commaSep:Function,commaSep1:Function}]]);z(ga,{GenLanguageDef:Ba,GenTokenParser:sb,makeTokenParser:tb});K=P(":!#$%&*+./<=>?@\\^|-~");Ia=Ba.LanguageDef(xa,{commentStart:"",commentEnd:"",commentLine:"",nestedComments:true,identStart:[H,"<|>",p("_")].resolve(),identLetter:[J,"<|>",P("_'")].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:true});ha=Ba.LanguageDef(xa,{commentStart:"{-",commentEnd:"-}",commentLine:"--",nestedComments:true,identStart:H,identLetter:[J,
"<|>",P,"_'"].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:true});J=Ba.LanguageDef(xa,{commentStart:"/*",commentEnd:"*/",commentLine:"//",nestedComments:true,identStart:H,identLetter:[J,"<|>",P,"_'"].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:false});H=ha.update({reservedOpNames:["::","..","=","\\","|","<-","->","@","~","=>"],reservedNames:["let","in","case","of","if","then","else","data","type","class","default","deriving",
"do","import","infix","infixl","infixr","instance","module","newtype","where","primitive"]});var uc=H.update({identLetter:[H.identLetter,"<|>",p("#")].resolve(),reservedNames:H.reservedNames.concat(["foreign","import","export","primitive","_ccall_","_casm_","forall"])}),vc=J.update({reservedNames:["case","class","default","extends","import","in","let","new","of","package"],caseSensitive:true});z(ga,{emptyDef:Ia,haskellStyle:ha,javaStyle:J,haskellDef:uc,mondrianDef:vc,getHaskell:function(){return tb(uc)},
getMondrian:function(){return tb(vc)}})})();
