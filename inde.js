//web pages that use InDependent should load this script and Ramda (http://ramdajs.com/)

function Universe(i){
    return new $PRIMUniverse(i)
}

function $PRIMUniverse(i){
    this[0] = "1"
    this[1] = i
    this.__defineGetter__("inDeType", function(){ //TODO: best name for type property?
        return Universe(i+1)
    });
}

function PiType(at,rt){
    return new $PRIMPiType(at, rt)
}

function $PRIMPiType(at, rt){
    this[0] = "2"
    this[1] = at
    this[2] = rt
    res.__defineGetter__("inDeType", function(){
        return Math.max(at.InDeType[1], rt.InDeType[1])
    });
}

//tests whether a and b are structurally equivalent
var eq = R.curry(function(a,b){
                    //if not, use implicit arg notation in code
    if(a.constructor === b.constructor){
        if(a.inDeType !== undefined && b.inDeType !== undefined){
            if(!eq(a.inDeType, b.inDeType)) return false;
            for(var i = 0; i <= a[0]; i++){
                if(!eq(a[i], b[i])){
                     return false;
                }
            }
            return true;
        }else{
            return a === b;//primitives and non-InDependent objects rely on JS equality
        }
    }
    
    return false;
    
});

//tests whether obj is an instance of the given ADT constructor
var matchADT = R.curry(function(obj, adt){
    return obj.isInDependent && adt.isInDependent && obj.constructor === adt.constructor
});
