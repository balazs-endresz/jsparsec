//import functions from JSParsec
//TODO: this should be removed

var ns = [];
for(var name in JSParsec)
    ns.push(name);

ns = JSParsec.map(function(name){
    return "var " + name + " = JSParsec." + name;
}, ns).join(";");

eval(ns);
