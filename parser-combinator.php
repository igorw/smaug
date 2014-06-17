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

function identity($x) {
    return $x;
}

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

// ---

function run_parser($parser, $str) {
    $tramp = new Trampoline();
    $results = [];

    $parser($str, $tramp, $result ==> {
        if ($result instanceof Success && $result->rest === "") {
            $results[] = $result;
        }
    });

    $out = function () use (&$results) {
        while ($tramp->has_next()) {
            $tramp->step();
            foreach ($results as $result) {
                yield $result;
            }
            $results = [];
        }
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
            if ($k === $key) {
                return $v;
            }
        }
        return null;
    }
    function remove($key) {
        foreach ($this->data as $i => list($k, $v)) {
            if ($k === $key) {
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
        return in_array($result, $entry->results, true);
    }
    function is_empty() {
        return $this->continuations === [] && $this->results === [];
    }
}

class Trampoline {
    // todo move to bottom
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
            // $table->put($fn, $memo);
            return $entry;
        }
        // first time parser has been called
        $entry = new Entry();
        $memo = new Table();
        $memo->put($str, $entry);
        $table->put($fn, $memo);
        return $entry;
    }
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
            $fn($args);
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
}

function succeed($val) {
    return memofn([$val], $val ===>
        ($str, $tramp, $cont) ==> {
            $cont(success($val, $str));
        });
}

function string($match) {
    return memofn([$match], $match ===>
        ($str, $tramp, $cont) ==> {
            $len = min(strlen($str), strlen($match));
            $head = (string) substr($str, 0, $len);
            $tail = (string) substr($str, $len);
            if ($head === $match) {
                $cont(success($head, $tail));
            } else {
                $cont(failure($tail));
            }
        });
}

function regexp($pattern) {
    return memofn([$pattern], $pattern ===>
        ($str, $tramp, $cont) ==> {
            preg_match('/^'.$pattern.'/', $str, $matches);
            if (count($matches) > 0) {
                list($match) = $matches[0];
                $end = strlen($match);
                $len = strlen($str);
                $head = (string) substr($str, 0, $end);
                $head = (string) substr($str, $end, $len);
                $cont(success($head, $tail));
            } else {
                $cont(failure($str));
            }
        });
}

__HALT_COMPILER();

(define (bind p fn)
  (lambda (str tramp cont)
    (p str tramp
       (lambda (result)
         (match result
           [(success val rest)
            ((fn val) rest tramp cont)]
           [failure
            (cont failure)])))))

(define seq
  (memo
   (lambda parsers
     (define (seq2 b a)
       (bind a (lambda (x)
                 (bind b (lambda (y)
                           (succeed (append x (list y))))))))
     (foldl seq2 (succeed '()) parsers))))

(define alt
  (memo
   (lambda parsers
     (lambda (str tramp cont)
       (for ((fn parsers))
            (send tramp push fn str cont))))))

(define red
  (memo
   (lambda (p fn)
     (bind p (lambda (val)
               (match val
                 [(list val ...) (succeed (apply fn val))]
                 [_ (succeed (fn val))]))))))

(define-parser expr
  (alt (red (seq expr (string "+") term)
            (lambda (x _ y) (+ x y)))
       (red (seq expr (string "-") term)
            (lambda (x _ y) (- x y)))
       term))

(define-parser term
  (alt (red (seq term (string "*") factor)
            (lambda (x _ y) (* x y)))
       (red (seq term (string "/") factor)
            (lambda (x _ y) (/ x y)))
       factor))

(define-parser factor
  (alt (red (seq (string "(") expr (string ")"))
            (lambda (_ x __) x))
       num))

(define-parser num
  (red (regexp "[0-9]+")
       string->number))

(stream->list (expr "1*2+3*4"))
(stream->list (expr "9-(5+2)"))
