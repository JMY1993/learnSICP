


let makeList = (a,b) => makePair(a,makeCdr(b));
let makeCdr = (atom)=>makePair(atom,null);

let makePair = (car, cdr) => (indicator)=>indicator=="car"?car:indicator=="cdr"?cdr:null;
let car = (pair)=>pair("car");
let cdr = (pair)=>pair("cdr");

let testList = makeList(1,2)