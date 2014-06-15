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

function bind($p, $fn) {
    return $str ==> {
        $result = $p($str);
        if ($result instanceof Success) {
            return $fn($result->value)->__invoke($result->rest);
        }
        return $result;
    };
}

function seq($a, $b) {
    return bind($a, $x ==>
                bind($b, $y ==>
                    succeed([$x, $y])));
}

// var_dump(seq(string('foo'), string('bar'))->__invoke('foobar'));
