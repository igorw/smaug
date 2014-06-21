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

function Y($le) {
    return call_user_func(
        $f ==> $f($f),
        $f ==> $le($x ==> call_user_func($f($f), $x))
    );
}

// ---

function memo(callable $fn) {
    $alist = [];
    return () ==> {
        $args = func_get_args();
        foreach ($alist as list($a, $result)) {
            if ($a == $args) {
                return $result;
            }
        }
        $result = call_user_func_array($fn, $args);
        array_unshift($alist, [$args, $result]);
        return $result;
    };
}

function memofn(array $args, callable $fn) {
    static $memos = [];
    $id = spl_object_hash($fn);
    $memo = isset($memos[$id]) ? $memos[$id] : memo($fn);
    return call_user_func_array($memo, $args);
}

// ---

function run_parser($parser, $str) {
    $tramp = new Trampoline();
    $results = [];

    $parser($str, $tramp, function ($result) use (&$results) {
        if ($result instanceof Success && $result->rest === '') {
            $results[] = $result;
        }
    });

    $out = function () use ($tramp, &$results) {
        do {
            $tramp->step();
            foreach ($results as $result) {
                yield $result;
            }
            $results = [];
        } while ($tramp->has_next());
    };

    return $out();
}

// crappy linear time lookup table
class Table {
    public $data = [];
    function put($key, $value) {
        $this->remove($key);
        $this->data[] = [$key, $value];
    }
    function lookup($key) {
        foreach ($this->data as list($k, $v)) {
            if ($k == $key) {
                return $v;
            }
        }
        return null;
    }
    function remove($key) {
        foreach ($this->data as $i => list($k, $v)) {
            if ($k == $key) {
                unset($this->data[$i]);
            }
        }
    }
}

class Entry {
    public $continuations = [];
    public $results = [];
    function push_continuation($cont) {
        array_unshift($this->continuations, $cont);
    }
    function push_result($result) {
        array_unshift($this->results, $result);
    }
    function result_subsumed($result) {
        return in_array($result, $this->results);
    }
    function is_empty() {
        return $this->continuations === [] && $this->results === [];
    }
}

class Trampoline {
    public $stack;
    public $table;
    function __construct(SplStack $stack = null, Table $table = null) {
        $this->stack = $stack ?: new SplStack();
        $this->table = $table ?: new Table();
    }
    function has_next() {
        return count($this->stack) > 0;
    }
    function step() {
        if ($this->has_next()) {
            list($fn, $args) = $this->stack->pop();
            call_user_func_array($fn, $args);
        }
    }
    function push_stack(/* $fn, $args */) {
        $args = func_get_args();
        $fn = array_shift($args);
        $this->stack->push([$fn, $args]);
    }
    function push($fn, $str, $cont) {
        $entry = $this->table_ref($fn, $str);
        if ($entry->is_empty()) {
            $entry->push_continuation($cont);
            // push the parser on the stack
            $this->push_stack($fn, $str, $this, $result ==> {
                if (!$entry->result_subsumed($result)) {
                    $entry->push_result($result);
                    foreach ($entry->continuations as $cont) {
                        $cont($result);
                    }
                }
            });
        } else {
            $entry->push_continuation($cont);
            foreach ($entry->results as $result) {
                $cont($result);
            }
        }
    }
    private function table_ref($fn, $str) {
        $memo = $this->table->lookup($fn);
        if ($memo) {
            $entry = $memo->lookup($str);
            if ($entry) {
                // parser has been called with str before
                return $entry;
            }
            // first time parser has been called with str
            $entry = new Entry();
            $memo->put($str, $entry);
            // this happens implicitly:
            // $this->table->put($fn, $memo);
            return $entry;
        }
        // first time parser has been called
        $entry = new Entry();
        $memo = new Table();
        $memo->put($str, $entry);
        $this->table->put($fn, $memo);
        return $entry;
    }
}

function succeed($val) {
    static $fn;
    $fn = $fn ?: $val ==>
        ($str, $tramp, $cont) ==> {
            $cont(success($val, $str));
        };

    return memofn([$val], $fn);
}

function string($match) {
    static $fn;
    $fn = $fn ?: $match ==>
        ($str, $tramp, $cont) ==> {
            $len = min(strlen($str), strlen($match));
            $head = (string) substr($str, 0, $len);
            $tail = (string) substr($str, $len);
            if ($head === $match) {
                $cont(success($head, $tail));
            } else {
                $cont(failure($tail));
            }
        };

    return memofn([$match], $fn);
}

function regexp($pattern) {
    static $fn;
    $fn = $fn ?: $pattern ==>
        ($str, $tramp, $cont) ==> {
            preg_match('/^'.$pattern.'/', $str, $matches);
            if (count($matches) > 0) {
                $match = $matches[0];
                $end = strlen($match);
                $len = strlen($str);
                $head = (string) substr($str, 0, $end);
                $tail = (string) substr($str, $end, $len);
                $cont(success($head, $tail));
            } else {
                $cont(failure($str));
            }
        };

    return memofn([$pattern], $fn);
}

function bind($p, $fn) {
    return ($str, $tramp, $cont) ==>
        $p($str, $tramp,
            $result ==> {
                if ($result instanceof Success) {
                    call_user_func($fn($result->value), $result->rest, $tramp, $cont);
                } else {
                    // failure
                    $cont($result);
                }
            });
}

function seq(/* $parsers... */) {
    static $fn;
    $fn = $fn ?: (/* $_parsers... */) ==> {
        $_parsers = func_get_args();
        $seq2 = ($b, $a) ==>
            bind($a, $x ==>
                bind($b, $y ==>
                    succeed(array_merge($x, [$y]))));

        // foldl
        $acc = succeed([]);
        foreach ($_parsers as $parser) {
            $acc = $seq2($parser, $acc);
        }
        return $acc;
    };

    $parsers = func_get_args();
    return memofn($parsers, $fn);
}

function alt(/* $parsers... */) {
    static $fn;
    $fn = $fn ?: (/** $_parsers... */) ==> {
        $_parsers = func_get_args();
        return ($str, $tramp, $cont) ==> {
            foreach ($_parsers as $fn) {
                $tramp->push($fn, $str, $cont);
            }
        };
    };

    $parsers = func_get_args();
    return memofn($parsers, $fn);
}

function red($p, $rfn) {
    static $fn;
    $fn = $fn ?: ($p, $rfn) ==>
        bind($p, $val ==>
            succeed(call_user_func_array($rfn, $val)));

    return memofn([$p, $rfn], $fn);
}

function delay_parser($fn) {
    return ($str, $tramp, $cont) ==>
        call_user_func($fn(), $str, $tramp, $cont);
}

function take($n, $iter) {
    if ($n === 0) {
        yield break;
    }
    foreach ($iter as $v) {
        yield $v;
        $n--;
        if ($n === 0) {
            break;
        }
    }
}

$expr = delay_parser(function () use (&$expr, &$term, &$factor, &$num) {
    return alt(red(seq($expr, string('+'), $term),
                   ($x, $_, $y) ==> $x + $y),
               red(seq($expr, string('-'), $term),
                   ($x, $_, $y) ==> $x - $y),
               $term);
});

$term = delay_parser(function () use (&$expr, &$term, &$factor, &$num) {
    return alt(red(seq($term, string('*'), $factor),
                   ($x, $_, $y) ==> $x * $y),
               red(seq($term, string('/'), $factor),
                   ($x, $_, $y) ==> $x / $y),
               $factor);
});

$factor = delay_parser(function () use (&$expr, &$term, &$factor, &$num) {
    return alt(red(seq(string('('), $expr, string(')')),
                   ($_, $x, $__) ==> $x),
               $num);
});

$num = red(regexp('[0-9]+'),
           'intval');

// var_dump(iterator_to_array(run_parser(seq(string('foo'), string('bar')), 'foobar')));

$s = delay_parser(function () use (&$s) {
    return alt(seq($s, string('a')),
               string('a'));
});
// var_dump(iterator_to_array(take(1, run_parser($s, 'a'))));
// var_dump(iterator_to_array(take(1, run_parser($s, 'aa'))));
// var_dump(iterator_to_array(take(2, run_parser($s, 'a'))));

// var_dump(iterator_to_array(run_parser($num, '1')));
// var_dump(iterator_to_array(run_parser($num, '42')));

// var_dump(iterator_to_array(run_parser($factor, '42')));
// var_dump(iterator_to_array(run_parser($factor, '(42)')));

// var_dump(iterator_to_array(run_parser($expr, '42')));
// var_dump(iterator_to_array(run_parser($expr, '1*2+3*4')));
// var_dump(iterator_to_array(run_parser($expr, '9-(5+2)')));
