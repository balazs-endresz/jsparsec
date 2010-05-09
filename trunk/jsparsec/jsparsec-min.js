/*

 JSParsec - A parser combinator library for JavaScript

 Version: 1.0.1

 http://code.google.com/p/jsparsec/

 Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 Dual licensed under the MIT and GPL licenses.


 The initial implementation of some combinators and the memoization is derived from:
 http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html

 Most functions should behave like their counterparts in Parsec:
 http://www.haskell.org/haskellwiki/Parsec

*/
;(function(){function Y(a){function b(){var c=C(arguments);return c.length>=(a._length===undefined?a.length:a._length)?a.apply(null,c):function(){return b.apply(null,c.concat(C(arguments)))}}return b}function Ua(a){return function(){return a}}function ia(a){return xc.call(a)=="[object Array]"}function Va(a){return a!==x}function C(a,b){return yc.call(a,b||0)}function Hb(a,b,c){for(var d=0,f=c.length;d<f;++d)b=a(b,c[d]);return b}function ta(a,b,c){for(var d=c.length-1;d>-1;--d)b=a(c[d],b);return b}
function z(a,b){for(var c=[],d=0,f=b.length;d<f;++d)c[d]=a(b[d],d);return c}function Ib(a,b){for(var c=[],d=0,f=a.length;d<f;++d)b(a[d])&&c.push(a[d]);return c}function Wa(a,b){var c=a.length;if(!c)return-1;for(var d=0;d<c;d++)if(a[d]===b)return d;return-1}function Jb(a,b){var c=a.length;if(!c)return-1;for(c=c-1;c>-1;--c)if(a[c]===b)return c;return-1}function Xa(a,b){for(var c=[],d=0,f=Math.min(a.length,b.length);d<f;++d)c[d]=[a[d],b[d]];return c}function Ya(a){var b=typeof a;if(b=="object")return a.sort();
if(b=="string")return C(a).sort().join("")}function Ja(a,b){b=b===x?[]:b;var c=a[0],d=C(a,1);return!a.length?[]:Ka(c,b)?Ja(d,b):R(c,Ja(d,R(c,b)))}function Kb(a,b,c){if(c.Nothing)return a;if(c.Just)return b(c[0])}function Lb(a,b){return a===b?ua.EQ:a<=b?ua.LT:ua.GT}function A(a,b){for(var c in b)a[c]=b[c];return a}function Mb(a,b){return function(){return a(b.apply(null,arguments))}}function Nb(a,b){return function(c){var d=C(arguments,1);d.unshift(b(c));return a.apply(null,d)}}function Za(a){return function(b,
c){return a(c,b)}}function R(a,b){if(typeof a=="string"&&typeof b=="string")return a+b;return[a].concat(b)}function $a(a,b){if(typeof a=="string"&&typeof b=="string")return a+b;return a+b.join("")}function ab(a,b){for(var c=[],d=0;d<a;++d)c[d]=b;return c}function Ob(a){return-a}function La(a){return!a.length}function Ka(a,b){return(b.indexOf?b.indexOf(a):Wa(b,a))!=-1}function bb(a){return/^\s$/.test(a)}function Pb(a){return a.toUpperCase()==a}function Qb(a){return a.toLowerCase()==a}function Rb(a){return/^\w$/.test(a)}
function cb(a){return/^\w$/.test(a)&&/^\D$/.test(a)}function Sb(a){return/^\d$/.test(a)}function db(a){return/^[0-9A-Fa-f]$/.test(a)}function Tb(a){return/^[0-7]$/.test(a)}function eb(a){if(!db(a))throw"Data.Char.digitToInt: not a digit "+a;return parseInt(a,16)}function zc(a,b){return{indexOf:function(c){return c>=a&&c<=b?true:-1},toString:function(){return"range("+a+", "+b+")"}}}function Ac(){var a,b;z(function(c){b=c.split(".");a=window[b[0]]=window[b[0]]||{};z(function(d){a=a[d]=a[d]||{}},b.slice(1))},
arguments);return a}function Ub(){}function Bc(a,b){if(!/^[0-9]$/.test(b))return b;if(typeof a!="object")return b;var c=0;for(var d in a){if(a.hasOwnProperty(d)&&c==b)return d;c++}return-1}function Ma(){}function Vb(a){return function(){var b=[],c=this._recordset;if(ia(c)){for(var d=0;d in this;d++)b.push(a?c[d].name||c[d]:this[d]);b=b.join(" ")}else{for(d in c){var f=a?c[d].name||c[d]:this[d];if(!a&&f instanceof Function)f=f.constructor!=Function?f.constructor.name:"Function("+f.name+")";b.push(d+
" :: "+f)}c=ab(this._dataConstructor.length+2," ").join("");b="{"+b.join("\n"+c+",")+"\n"+c+"}"}return this._dataConstructor+" "+b}}function ja(a,b){function c(j,i){function r(L,D){var Na=L instanceof Ub,S=Na?D:C(arguments),I=new a,F=0;I.constructor=a;I._recordset=t&&i[0]||i;I._dataConstructor=j;I.update=function(va){var wa={};for(var Z in i[0]){wa[Z]=this[Z];if(Z in va)wa[Z]=va[Z]}return r(xa,wa)};I[j]=true;if(S!==x)for(var M in S)if(S.hasOwnProperty(M)&&M!=j){if(Na&&i&&t)if(!(M in i[0]))throw"The accessor '"+
M+"' is not defined for the data constructor '"+j+"'";var ka=Bc(i[0],M),la=S[F]!==undefined?S[F]:S[ka],$=t?i[0][ka]:i[F];if($.name&&!($==la.constructor||la instanceof $))throw"Type error: expecting '"+$.name+"' instead of '"+la.constructor.name+"' in the argument '"+(ka||F)+"' of the data constructor '"+j+"' of type '"+a.name+"'";I[ka]=I[F]=I[M]=S[M];F++}else if(M==j)throw"Accessor has the same name as the data constructor: '"+j+"'";return I}var t=i&&typeof i[0]=="object";return r}if(a.constructors)throw"Type constructor has been already defined: '"+
a.name+"'";a.constructors=b;a.prototype=new Ma;for(var d=0,f=b.length;d<f;++d){var g=typeof b[d]!="object",h=g?b[d]:b[d][0];if(h in{})throw"The name of the data constructor can't be a property of Object.prototype as well!";a[h]=g?c(h)():c(h,C(b[d],1));if(!g)a[h]._length=C(b[d],1).length}}function V(){}function ua(){}function ma(){}function T(a){return["l",a]}function aa(a){return["r",a]}function Wb(a){return["x",a]}function Cc(a){if(a)if(!a._String){var b=na[a];if(a._Op&&!b)return"l";return b&&(b.fixity[0]||
"l")}}function Dc(a){if(a)if(!a._String){var b=na[a];if(a._Op&&!b)return 9;return b&&(Va(b.fixity[1])?b.fixity[1]:9)}}function fb(a,b,c){var d;if(a[b]._Op){delete a[b]._Op;d=a[b]}else d=na[a[b]].func;d=d(a[b-1],a[b+1]);a.splice(b-1,3,d);return ba(a,c)}function Ec(a){a._Array=true;return a}function Fc(a){a=new String(a);a._String=true;return a}function Gc(a){a._Op=true;return a}function ba(a,b){a=z(function(i){if(i&&i._Array){delete i._Array;return i}return ia(i)?ba(i,b):i&&i.CallStream?i.resolve():
i},a);if(b)a=z(function(i){return i instanceof gb?b:i},a);for(var c=[],d=[],f=0,g=a.length;f<g;++f){var h=a[f],j=false;if(na[h])j=true;if(h&&h._String){j=false;h=h.toString()}if(h&&h._Op)j=true;if(!j&&f!=g-1)c.push(h);else{f==g-1&&c.push(h);c=c.length>1?c[0].apply(null,c.slice(1)):c[0];d.push(c);f!=g-1&&d.push(h);c=[]}}a=d;d=z(Cc,a);g=z(Dc,a);h=Ib(g,Va).sort().pop();f=Wa(g,h);g=Jb(g,h);return d[f]=="l"?fb(a,f,b):d[g]=="r"?fb(a,g,b):d[f]=="x"?fb(a,f,b):a[0]}function gb(){}function n(){function a(g,
h,j){return d(g,h,j)}function b(g,h,j){if(g instanceof ya)return(f?d:b.resolve())(g,h,j);c.push(ba(arguments,a));return b}var c=[],d,f;c.push(ba(arguments,a));b.resolve=function(){if(f)return d;d=k.apply(null,c);c=null;f=true;return d};b.CallStream=true;return b}function ya(a,b){this.input=a;this.index=b||0;this.length=a.length-this.index;this.cache={};return this}function Xb(a){return new ya(a)}function B(a,b,c){return{ast:a,success:b===x?true:b,expecting:c}}function oa(a){return B(x,false,a)}function Oa(a){return function(b,
c,d){return d(B(null,false,{unexpected:c[a]}))}}function Yb(a){return function(b,c,d){return d(B(x,false,a))}}function Zb(a,b,c){return c(B(x,false))}function hb(a){return typeof a=="string"?N(a):ia(a)?ba(a):a}function Hc(a){for(;a&&a.func;)a=a.func.apply(null,a.args||[])}function ib(a,b){b=b||0;b++;if(a&&a.func){a=a.func.apply(null,a.args||[]);b%500==0?setTimeout(function(){ib(a,b)},1):ib(a,b)}else b=0}function Ic(a,b,c,d,f){var g=b instanceof ya?b:Xb(b);(f?ib:Hc)({func:a,args:[g,{},function(h){h.state=
g;delete h.index;delete h.length;if(h.success){delete h.error;delete h.expecting}else{h.error=jb(h.expecting,h.state);d&&d(h.error)}c(h)}]})}function jb(a,b,c,d){c=c===undefined?b.index:c;if(typeof a=="string"){var f=b.input.split("\n"),g=f.length,h=b.input.substr(c).split("\n").length;g=g-h+1;f=c-f.splice(0,g-1).join("\n").length;b=d||b.input.substr(c,a.length).substr(0,6);return'Unexpected "'+(b.length?b:"end of file")+(d?"":'", expecting "'+a)+'" at line '+g+" char "+f}if(ia(a)){a=z(function(j){return typeof j==
"object"?j.expecting:j},a);return jb(a.join('" or "'),b)}else if(typeof a=="object")return jb(a.expecting,b,a.at,a.unexpected)}function o(){}function kb(a,b){return function(c,d,f){return{func:a,args:[c,d,function(g){return f(b(g))}]}}}function q(a,b){if(a=="scope")throw"Can't use 'scope' as an identifier!";return function(c,d,f){return{func:b,args:[c,d,function(g){if(g.success)d[a]=g.ast;return f(g)}]}}}function u(a,b){b&&C(arguments);return function(c,d,f){return{func:function(){var g;if(typeof a==
"string"){if(!(a in d))throw'Not in scope: "'+a+'"';g=d[a]}else g=a(d);return f(B(g))}}}}function lb(a){var b=C(arguments,1);return function(c){return a.apply(null,z(function(d){return c[d]},b))}}function $b(a,b,c){return c(B(a.index))}function ac(a){return function(b,c,d){b.scrollTo(c[a]);return d(Jc)}}function bc(a){return function(b,c,d){return d(B(a))}}function za(a,b){return pa(function(c){return c[0](c[1])},mb(a,b))}function cc(a,b){a=Y(a);return function(c,d,f){return{func:b,args:[c,d,function(g){if(!g.success)return f(g);
g=A({},g);g.ast=a(g.ast);return f(g)}]}}}function dc(a,b){return k(q("a",a),b,u("a"))}function w(a,b){function c(d,f,g){return{func:a,args:[d,f,function(h){function j(r){var t=r.expecting;if(t)if(ia(t))i=i.concat(t);else i.push(t);if(r.success)delete r.expecting;else r.expecting=i}var i=[];j(h);return h.ast!==undefined?{func:g,args:[h]}:{func:b,args:[d,f,function(r){j(r);return g(r)}]}}]}}c.constructor=o;return c}function Kc(){var a=z(hb,arguments);return function(b,c,d){for(var f=1,g=a.length,h=
a[0];f<g;++f)h=w(h,a[f]);return h(b,c,d)}}function mb(a){return function(b,c,d){function f(i){return function(r,t,L){return{func:i,args:[r,t,function(D){g++;if(!D.success)return L(D);D.ast!==x&&h.push(D.ast);return g<j?f(a[g])(r,t,L):L(D)}]}}}var g=0,h=[],j=a.length;return{func:f(a[g]),args:[b,c,function(i){i=A({},i);i.ast=h;i.success&&delete i.expecting;return d(i)}]}}}function ec(a){return function(b){return function(c,d,f){function g(i){return function(r,t,L){return{func:i,args:[r,t,function(D){if(!D.success)return L(D);
h=true;D.ast!==x&&j.push(D.ast);return g(i)(r,t,L)}]}}}var h=false,j=[];return{func:g(b),args:[c,d,function(i){i=A({},i);i.success=!a||h&&a;i.ast=j;i.success&&delete i.expecting;return f(i)}]}}}}function nb(a){return function(b){var c=fc++,d=function(f,g,h){g=f.index;var j=f.getCached(c);if(j!==x)return h(j);j=a(b,f,g);f.putCached(c,g,j);return h(j)};d.constructor=o;return d}}function Pa(a){return function(b,c){var d=fc++,f=function(g,h,j){var i=g.index,r=g.getCached(d);if(r!==x)return j(r);return{func:b,
args:[g,h,function(t){t=a(c,t,g,i);g.putCached(d,i,t);return j(t)}]}};f.constructor=o;return f}}function gc(a){function b(c,d){return k(q("x",c),q("xs",d),u(lb(R,"x","xs")))}return ta(b,m([]),a)}function ob(a){return ta(w,pb,a)}function Qa(a,b){return w(b,m(a))}function Lc(a){return Qa(V.Nothing,qb(V.Just,a))}function Mc(a){return w(k(a,m(null)),m(null))}function Ra(a){return k(a,W(a))}function rb(a,b){return w(Sa(a,b),m([]))}function Sa(a,b){return k(q("x",a),q("xs",ca(k(b,a))),O(R,"x","xs"))}function hc(a,
b){return k(q("x",a),w(k(b,q("xs",function(c,d,f){return ic(a,b)(c,d,f)}),u(function(c){return R(c.scope.x,c.xs)})),u(function(c){return[c.x]})))}function ic(a,b){return w(hc(a,b),m([]))}function Nc(a,b){return Aa(k(q("x",a),b,u("x")))}function Oc(a,b){return ca(k(q("x",a),b,u("x")))}function Pc(a,b){return a<=0?m([]):gc(ab(a,b))}function Qc(a,b,c){return w(jc(a,b),m(c))}function Rc(a,b,c){return w(kc(a,b),m(c))}function kc(a,b){function c(d){var f=k(q("f",b),q("y",a),function(g,h,j){return c(h.f(d,
h.y))(g,h,j)});return w(f,m(d))}return k(q("x",a),function(d,f,g){return c(f.x)(d,f,g)})}function jc(a,b){function c(f){var g=k(q("f",b),q("y",d),function(h,j,i){return i(B(j.f(f,j.y)))});return w(g,m(f))}var d=k(q("x",a),function(f,g,h){return c(g.x)(f,g,h)});return d}function Sc(a,b,c){b=a.at(0);if(b.length){a.scroll(1);return c(B(b))}return c(oa("anyToken"))}function Tc(a,b,c){return c(B(x,!a.length,a.length?"end of input":x))}function sb(a){return G(w(k(q("c",G(a)),Oa("c")),m(null)))}function Uc(a,
b){function c(f,g,h){return d(f,g,h)}var d=w(k(b,m([])),k(q("x",a),q("xs",c),O(R,"x","xs")));return d}function Vc(a){return k(q("state",$b),q("x",a),ac("state"),u("x"))}function Ba(){}function tb(){}function ub(a){function b(e){return c(N(e))}function c(e){return k(q("x",e),lc,u("x"))}function d(e,l,s){return mc(e,l,s)}function f(e,l,s){return nc(e,l,s)}function g(e,l,s){return vb(e,l,s)}function h(e){return da(b("("),b(")"),e)}function j(e){return da(b("{"),b("}"),e)}function i(e){return da(b("<"),
b(">"),e)}function r(e){return da(b("["),b("]"),e)}function t(e){return rb(e,wb)}function L(e){return rb(e,xb)}function D(e){return Sa(e,wb)}function Na(e){return Sa(e,xb)}function S(e){return k(p(e[0]),m(e[1]))}function I(e){return G(k(N(e[0]),m(e[1])))}function F(e,l){return n("digits","<-",Aa,l)(u,function(s){return Hb(function(E,Ca){return e*E+oc(eb(Ca))},0,s.digits)}).resolve()}function M(e,l,s){return ea(e,l,s)}function ka(e,l){return(l+pc(eb(e)))/10}function la(e){return e<0?1/la(-e):Da(Math.pow(10,
e))}function $(e){return[n("fract","<-",Wc)("expo","<-",Qa,1,qc)(u,function(l){return Da(e+l.fract)*l.expo}),"<|>",n("expo","<-",qc)(u,function(l){return Da(e)*l.expo})].resolve()}function va(e){return n("f","<-",$,e)(O,ma.Right,"f").resolve()}function wa(e){return[c,"$",G,"$",n(N(e))(sb,a.opLetter,"<?>","end of "+e)].resolve()}function Z(e){return rc(Ya(a.reservedOpNames),e)}function Xc(e){function l(E){return!E.length?m(null):k(Ea(s(E[0]),""+e),l(C(E,1)))}function s(E){return cb(E)?w(p(E.toLowerCase()),
p(E.toUpperCase())):p(E)}return a.caseSensitive?N(e):k(l(e),m(e))}function Yc(e){return[c,"$",G,"$",n(Xc(e))(sb,a.identLetter,"<?>","end of "+e)].resolve()}function rc(e,l){function s(E){if(!E.length)return false;var Ca=Lb(E[0],l);return Ca.LT?s(C(E,1)):Ca.EQ?true:Ca.GT?false:null}return s(e)}function Zc(e){e=a.caseSensitive?e:e.toLowerCase();return rc($c,e)}if(!a.LanguageDef)throw"Type error: unexpected '"+a.constructor.name+"', expecting 'GenLanguageDef.LanguageDef'";var qa=Ra(y(bb)),Ta=n(G(N(a.commentLine)))(W,
y(function(e){return e!="\n"}))(m,null).resolve(),v=Ja(C(a.commentEnd+a.commentStart)),mc=[k(G(N(a.commentEnd)),m(null)),"<|>",k(Ra(yb(v)),d),"<|>",k(P(v),d),"<?>","end of comment"].resolve(),nc=[k(G(N(a.commentEnd)),m(null)),"<|>",k(g,f),"<|>",k(Ra(yb(v)),f),"<|>",k(P(v),f),"<?>","end of comment"].resolve();v=a.nestedComments?nc:mc;var vb=k(G(N(a.commentStart)),v);v=La(a.commentLine);var Q=La(a.commentStart),lc=(v&&Q?[W,[qa,"<?>",""]]:v?[W,[qa,"<|>",vb,"<?>",""]]:Q?[W,[qa,"<|>",Ta,"<?>",""]]:[W,
[qa,"<|>",Ta,"<|>",vb,"<?>",""]]).resolve(),xb=b(";"),wb=b(",");qa=b(".");Ta=b(":");v=Xa("abfnrtv\\\"'","a\u0008\u000c\n\r\t\u000b\\\"'");Q=Xa(["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","SUB","ESC","DEL"]+["BS","HT","LF","VT","FF","CR","SO","SI","EM","FS","GS","RS","US","SP"],["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL","DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","SUB","ESC","DEL"]+["BS","HT","LF","VT","FF","CR","SO","SI","EM",
"FS","GS","RS","US","SP"]);v=ob(z(S,v));var U=ob(z(I,Q));Q=y(function(e){return e!='"'&&e!="\\"&&e>"\u0016"});var fa=p("&"),Fa=n(Aa,zb)(p("\\"),"<?>","end of string gap").resolve(),Ga=n("code","<-",M,"<|>",k(p("o"),F(8,Ab)),"<|>",k(p("x"),F(16,Bb)))(u(function(e){return toEnum(Da(e.code))})).resolve(),X=n(p("^"))("code","<-",sc)(u(function(e){return toEnum(fromEnum(e.code)-fromEnum("A"))})).resolve();U=[v,"<|>",Ga,"<|>",U,"<|>",X,"<?>","escape code"].resolve();v=k(p("\\"),U);v=[y(function(e){return e!=
"'"&&e!="\\"&&e>"\u0016"}),"<|>",v,"<?>","literal character"].resolve();v=[c,[da,p("'"),[p("'"),"<?>","end of character"],v],"<?>","character"].resolve();fa=n(p("\\"))(n(Fa)(m,V.Nothing),"<|>",n(fa)(m,V.Nothing),"<|>",n("esc","<-",U)(O,V.Just,"esc")).resolve();Q=[n("c","<-",Q)(O,V.Just,"c"),"<|>",fa,"<?>","string character"].resolve();Q=c([n("str","<-",da,p('"'),[p('"'),"<?>","end of string"],[ca,Q])(u,function(e){return ta(Y(Kb)(ra,Y(R)),"",e.str)}),"<?>","literal string"].resolve());var ea=F(10,
Cb);fa=n(P,"xX")(F,16,Bb).resolve();Fa=n(P,"oO")(F,8,Ab).resolve();var Wc=[n(p("."))("digits","<-",Aa,Cb,"<?>","fraction")(u,function(e){return ta(ka,0,e.digits)}),"<?>","fraction"].resolve();U=[[p("-"),">>",m,Ob],"<|>",[p("+"),">>",m,ra],"<|>",m,ra].resolve();var qc=[n(P,"eE")("f","<-",U)("e","<-",ea,"<?>","exponent")(O,la,"f","e"),"<?>","exponent"].resolve();Ga=n("n","<-",ea)(function(e,l,s){return $(l.n)(e,l,s)}).resolve();X=n("n","<-",ea)(function(e,l,s){return Qa(ma.Left(l.n),va(l.n))(e,l,s)}).resolve();
var sa=[n("n","<-",fa,"<|>",Fa)(O,ma.Left,"n"),"<|>",X,"<|>",va(0),"<|>",m,ma.Left(0)].resolve(),Ha=[k(p("0"),sa),"<|>",X].resolve();sa=[[n(p,"0")(fa,"<|>",Fa,"<|>",ea,"<|>",m,0),"<?>",""].resolve(),"<|>",ea].resolve();X=n("f","<-",c,U)("n","<-",sa)(u,function(e){return e.f(e.n)});U=[c,Ha,"<?>","number"].resolve();Ga=[c,Ga,"<?>","float"].resolve();X=[c,X,"<?>","integer"].resolve();sa=[c,sa,"<?>","natural"].resolve();Ha=[n("c","<-",a.opStart)("cs","<-",ca,a.opLetter)(O,$a,"c","cs"),"<?>","operator"].resolve();
Ha=[c,"$",G,"$",n("name","<-",Ha)(function(e,l,s){return(Z(l.name)?Oa("reserved operator "+l.name):m(l.name))(e,l,s)})].resolve();var Db=[n("c","<-",a.identStart)("cs","<-",ca,a.identLetter)(O,$a,"c","cs"),"<?>","identifier"].resolve();Db=[c,"$",G,"$",n("name","<-",Db)(function(e,l,s){return(Zc(l.name)?Oa("reserved word "+l.name):m(l.name))(e,l,s)})].resolve();var tc=Ya(a.reservedNames),$c=a.caseSensitive?tc:z(function(e){return e.toLowerCase()},tc);return tb.TokenParser(xa,{identifier:Db,reserved:Yc,
operator:Ha,reservedOp:wa,charLiteral:v,stringLiteral:Q,natural:sa,integer:X,float_:Ga,naturalOrFloat:U,decimal:ea,hexadecimal:fa,octal:Fa,symbol:b,lexeme:c,whiteSpace:lc,parens:h,braces:j,angles:i,brackets:r,squares:r,semi:xb,comma:wb,colon:Ta,dot:qa,semiSep:L,semiSep1:Na,commaSep:t,commaSep1:D})}var x,xc={}.toString,yc=[].slice,ra=function(a){return a},J=Y(function(a,b){return a(b)}),oc=parseInt,Da=ra,pc=ra,ga=window.JSParsec={};A(ga,{curry:Y,const_:Ua,"const":Ua,isArray:ia,isDefined:Va,slice:C,
foldl:Hb,foldr:ta,map:z,filter:Ib,indexOf:Wa,lastIndexOf:Jb,zip:Xa,sort:Ya,nub:Ja,maybe:Kb,compare:Lb,compose:Mb,compose1:Nb,call:J,id:ra,flip:Za,cons:R,consJoin:$a,replicate:ab,negate:Ob,null_:La,"null":La,elem:Ka,isSpace:bb,isUpper:Pb,isLower:Qb,isAlpha:cb,isAlphaNum:Rb,isDigit:Sb,isHexDigit:db,isOctDigit:Tb,digitToInt:eb,range:zc,extend:A,namespace:Ac,toInteger:oc,fromInteger:Da,fromIntegral:pc});var xa=new Ub;Ma.prototype.toString=Vb();Ma.prototype.dataConstructorToString=Vb(true);ja(V,[["Just",
"a"],"Nothing"]);ja(ua,["LT","EQ","GT"]);ja(ma,[["Left","a"],["Right","b"]]);var na={$:{func:J,fixity:aa(0)},".":{func:Nb,fixity:aa(9)},":":{func:R,fixity:aa(5)}};Array.prototype.resolve=function(){return ba(this)};var H=new gb;A(ga,{data:ja,ADT:Ma,Maybe:V,Ordering:ua,Either:ma,operators:na,infix:Wb,infixl:T,infixr:aa,arr:Ec,op:Gc,str:Fc,resolve:ba,recurse:H,Recurse:gb,cs:n});ya.prototype={memoize:false,scrollTo:function(a){this.index=a;this.length=this.input.length-a;return this},scroll:function(a){this.index+=
a;this.length-=a;return this},at:function(a){return this.input.charAt(this.index+a)},substring:function(a,b){return this.input.substring(a+this.index,(b||this.length)+this.index)},substr:function(a,b){return this.input.substring(a+this.index,b||this.length)},toString:function(){var a=this.substring(0);return"PS at "+this.index+" "+(a.length?'"'+a+'"':"Empty")},getCached:function(a){if(this.memoize)if(a=this.cache[a])if(a=a[this.index]){this.index=a.index;this.length=a.length;return a}},putCached:function(a,
b,c){if(!this.memoize)return false;c.index=this.index;c.length=this.length;var d=this.cache[a];d||(d=this.cache[a]={});d[b]=c}};var Jc=B(x),pb=Zb;H=pb;var fc=0,Eb=function(a,b){function c(d,f,g){return{func:a,args:[d,f,function(h){return h.success?b(d,f,g):g(h)}]}}c.constructor=o;return c},k=function(){function a(c,d,f){var g={},h=1,j=b.length,i=b[0];for(g.scope=d;h<j;++h)i=Eb(i,b[h]);return i(c,g,f)}var b=z(hb,arguments);a.constructor=o;return a},O=Mb(u,lb),m=bc,uc=m,pa=cc,qb=pa,Ia=qb,ha=function(a,
b,c){return za(pa(a,b),c)},K=Eb,Fb=w,ca=ec(false),Aa=ec(true),G=Pa(function(a,b,c,d){if(b.success)return b;c.scrollTo(d);return oa(b.expecting)}),W=function(a){return Pa(function(b,c){c=A({},c);c.ast=x;return c})(ca(a),null)},p=nb(function(a,b){if(b.length>0&&b.at(0)==a){b.scroll(1);return B(a)}return oa(a)}),y=nb(function(a,b){var c=b.at(0);if(b.length>0&&a(c)){b.scroll(1);return B(c)}return oa(c)}),N=function(a){return Pa(function(b,c,d,f){c.ast=c.ast.join("");c=A({},c);if(c.success)delete c.expecting;
else c.expecting={at:f,expecting:a};if(!c.ast.length)c.ast=x;return c})(mb(z(p,a)),null)},Ea=Pa(function(a,b,c,d){if(!b.success){b=A({},b);b.expecting={at:d,expecting:a}}return b}),Gb=nb(function(a,b){var c;if(typeof a=="string")if(b.substring(0,a.length)==a){b.scroll(a.length);c=B(a)}else c=oa(a);else if(a.exec){var d=new RegExp("^"+a.source);c=b.substring(0);d=(d=(d=d.exec(c))&&d[0])&&d.length;c=c.substr(0,d);if(d){b.scroll(d);c=B(c)}else c=oa(a.source)}return c});A(na,{"<-":{func:q,fixity:aa(-1)},
">>=":{func:kb,fixity:T(1)},"=<<":{func:Za(kb),fixity:aa(1)},">>":{func:K,fixity:T(1)},"*>":{func:K,fixity:T(4)},"<*":{func:dc,fixity:T(4)},"<$>":{func:pa,fixity:T(4)},"<*>":{func:za,fixity:T(4)},"<**>":{func:Y(ha)(Za(J)),fixity:T(4)},"<$":{func:function(a,b){return dc(uc(value),b)},fixity:T(4)},"<|>":{func:w,fixity:aa(1)},"<?>":{func:Ea,fixity:Wb(0)}});A(ga,{sequence:gc,run:Ic,Parser:o,ParseState:ya,ps:Xb,toParser:hb,unexpected:Oa,parsecMap:cc,fmap:pa,liftM:qb,liftA:Ia,liftA2:ha,liftA3:function(a,
b,c,d){return za(za(pa(a,b),c),d)},ap:za,parserBind:kb,parserReturn:bc,return_:m,pure:uc,parserFail:Yb,fail:Yb,parserZero:Zb,mzero:pb,empty:H,parserPlus:w,parserPlusN:Kc,mplus:Fb,do_:k,do2:Eb,bind:q,ret:u,withBound:lb,returnCall:O,getParserState:$b,setParserState:ac,tokens:mb,many:ca,many1:Aa,string:N,char_:p,satisfy:y,label:Ea,try_:G,skipMany:W,match:Gb});var P=function(a){return Ea(y(function(b){return Ka(b,a)}),"oneOf("+a+")")},yb=function(a){return Ea(y(function(b){return!Ka(b,a)}),"noneOf("+
a+")")},zb=[y,bb,"<?>","space"].resolve();Ia=[W,zb,"<?>","white space"].resolve();ha=[p,"\n","<?>","new-line"].resolve();K=[p,"\t","<?>","tab"].resolve();var sc=[y,Pb,"<?>","uppercase letter"].resolve();Fb=[y,Qb,"<?>","lowercase letter"].resolve();J=[y,Rb,"<?>","letter or digit"].resolve();H=[y,cb,"<?>","letter"].resolve();var Cb=[y,Sb,"<?>","digit"].resolve(),Bb=[y,db,"<?>","hexadecimal digit"].resolve(),Ab=[y,Tb,"<?>","octal digit"].resolve();Gb=[y,Ua(true)].resolve();A(ga,{oneOf:P,noneOf:yb,space:zb,
spaces:Ia,newline:ha,tab:K,upper:sc,lower:Fb,alphaNum:J,letter:H,digit:Cb,hexDigit:Bb,octDigit:Ab,anyChar:Gb});var da=Y(function(a,b,c){return k(a,q("x",c),b,u("x"))});A(ga,{choice:ob,count:Pc,between:da,option:Qa,optionMaybe:Lc,optional:Mc,skipMany1:Ra,sepBy:rb,sepBy1:Sa,endBy:Oc,endBy1:Nc,sepEndBy:ic,sepEndBy1:hc,chainl:Rc,chainl1:kc,chainr:Qc,chainr1:jc,eof:Tc,notFollowedBy:sb,manyTill:Uc,lookAhead:Vc,anyToken:Sc});ja(Ba,[["LanguageDef",{commentStart:String,commentEnd:String,commentLine:String,
nestedComments:Boolean,identStart:o,identLetter:o,opStart:o,opLetter:o,reservedNames:Array,reservedOpNames:Array,caseSensitive:Boolean}]]);ja(tb,[["TokenParser",{identifier:o,reserved:Function,operator:o,reservedOp:Function,charLiteral:o,stringLiteral:o,natural:o,integer:o,float_:o,naturalOrFloat:o,decimal:o,hexadecimal:o,octal:o,symbol:Function,lexeme:Function,whiteSpace:o,parens:Function,braces:Function,angles:Function,brackets:Function,squares:Function,semi:o,comma:o,colon:o,dot:o,semiSep:Function,
semiSep1:Function,commaSep:Function,commaSep1:Function}]]);A(ga,{GenLanguageDef:Ba,GenTokenParser:tb,makeTokenParser:ub});K=P(":!#$%&*+./<=>?@\\^|-~");Ia=Ba.LanguageDef(xa,{commentStart:"",commentEnd:"",commentLine:"",nestedComments:true,identStart:[H,"<|>",p("_")].resolve(),identLetter:[J,"<|>",P("_'")].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:true});ha=Ba.LanguageDef(xa,{commentStart:"{-",commentEnd:"-}",commentLine:"--",nestedComments:true,identStart:H,identLetter:[J,
"<|>",P,"_'"].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:true});J=Ba.LanguageDef(xa,{commentStart:"/*",commentEnd:"*/",commentLine:"//",nestedComments:true,identStart:H,identLetter:[J,"<|>",P,"_'"].resolve(),opStart:K,opLetter:K,reservedOpNames:[],reservedNames:[],caseSensitive:false});H=ha.update({reservedOpNames:["::","..","=","\\","|","<-","->","@","~","=>"],reservedNames:["let","in","case","of","if","then","else","data","type","class","default","deriving",
"do","import","infix","infixl","infixr","instance","module","newtype","where","primitive"]});var vc=H.update({identLetter:[H.identLetter,"<|>",p("#")].resolve(),reservedNames:H.reservedNames.concat(["foreign","import","export","primitive","_ccall_","_casm_","forall"])}),wc=J.update({reservedNames:["case","class","default","extends","import","in","let","new","of","package"],caseSensitive:true});A(ga,{emptyDef:Ia,haskellStyle:ha,javaStyle:J,haskellDef:vc,mondrianDef:wc,getHaskell:function(){return ub(vc)},
getMondrian:function(){return ub(wc)}})})();
