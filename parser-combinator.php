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
    return memofn([$value], () ==>
            memo($str ==>
                success($value, $str)));
}

// var_dump(succeed([])->__invoke("foo"));

function string($match) {
    return memofn(
        [$match],
        () ==> memo($str ==> {
            $len = min(strlen($str), strlen($match));
            $head = (string) substr($str, 0, $len);
            $tail = (string) substr($str, $len);

            if ($head == $match) {
                return success($head, $tail);
            }

            return failure($str);
        })
    );
}

// var_dump(string('foo')->__invoke('foobar'));
// var_dump(string('foo')->__invoke('bar'));

function alt($a, $b) {
    return memofn(
        [$a, $b],
        () ==> memo($str ==> {
            $result = $a($str);
            if ($result instanceof Success) {
                return $result;
            }
            return $b($str);
        })
    );
}

function bind($p, callable $fn) {
    return $str ==> {
        $result = $p($str);
        if ($result instanceof Success) {
            return $fn($result->value)->__invoke($result->rest);
        }
        return $result;
    };
}

function seq($a, $b) {
    return memofn(
        [$a, $b],
        () ==> memo(bind($a, $x ==>
                    bind($b, $y ==>
                        succeed([$x, $y]))))
    );
}

// var_dump(seq(string('foo'), string('bar'))->__invoke('foobar'));

function memo(callable $fn) {
    $alist = [];
    return () ==> {
        $args = func_get_args();
        foreach ($alist as list($a, $result)) {
            if ($a === $args) {
                return $result;
            }
        }
        $result = call_user_func_array($fn, $args);
        array_unshift($alist, [$args, $result]);
        return $result;
    };
}

function memofn(array $args, callable $fn) {
    return call_user_func_array(memo($fn), $args);
}

function Y($le) {
    return call_user_func(
        $f ==> $f($f),
        $f ==> $le($x ==> call_user_func($f($f), $x))
    );
}

// $r = Y($r ==>
//         alt(seq(string('a'), $r),
//             string('a')));
// var_dump($r('a'));
// var_dump($r('aa'));
// var_dump($r('aaa'));

// left recursion fails
// $s = Y($s ==>
//         alt(seq($s, string('a')),
//             string('a')));
// var_dump($s('a'));
