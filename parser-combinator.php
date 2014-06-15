<?hh

class Success {
    public $value;
    public $rest;
    function __construct($value, $rest) {
        $this->value = $value;
        $this->rest = $rest;
    }
}

class Failure {
    public $rest;
    function __construct($rest) {
        $this->rest = $rest;
    }
}

function success($value, $rest) {
    return new Success($value, $rest);
}

function failure($rest) {
    return new Failure($rest);
}

// ---

function succeed($value) {
    return $str ==>
        success($value, $str);
}

// var_dump(succeed([])->__invoke("foo"));

function string($match) {
    return $str ==> {
        $len = min(strlen($str), strlen($match));
        $head = substr($str, 0, $len);
        $tail = substr($str, $len);

        if ($head == $match) {
            return success($head, $tail);
        }

        return failure($str);
    };
}

// var_dump(string('foo')->__invoke('foobar'));
// var_dump(string('foo')->__invoke('bar'));

function alt($a, $b) {
    return $str ==> {
        $result = $a($str);
        if ($result instanceof Success) {
            return $result;
        }
        return $b($str);
    };
}

function seq($a, $b) {
    return $str ==> {
        $resa = $a($str);
        if ($resa instanceof Success) {
            $val1 = $resa->value;
            $rest1 = $resa->rest;

            $resb = $b($rest1);
            if ($resb instanceof Success) {
                $val2 = $resb->value;
                $rest2 = $resb->rest;
                return success([$val1, $val2], $rest2);
            }
            return $resb;
        }
        return $resa;
    };
}
