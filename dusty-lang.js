//web pages that use Dusty should load this script and Ramda (http://ramdajs.com/)

Object.prototype.isDusty = false

function Universe(i){
    //TODO: are these needed?
    this[0] = "1"
    this[1] = i
    // this.__defineGetter__("dustytype", function(){
    //     return Universe(i+1)
    // });
    this.isDusty = true
    return this
}

function PiType(at, rt){
    this[0] = "2"
    this[1] = at
    this[2] = rt
    // res.__defineGetter__("dustytype", function(){
    //     return Math.max(at.dustytype[1], rt.dustytype[1])
    // });
    this.isDusty = true
    return this
}

function eq(a,b){
    if(a.constructor === b.constructor){
        if(a.isDusty && b.isDusty){
            for(var i = 1; i <= a[0]; i++){
                if(!eq(a[i], b[i])) return false;
            }
        }else{
            return a === b;//primitives and non-Dusty objects
        }
    }
    
    return false;
    
}
