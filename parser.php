<?hh

namespace igorw\gll;

// create a new Parser object:
//   - kind: type of parser, in one word ("alt", "optional", ...)
//   - nested: list of nested parsers, if this is a combiner
//   - describer: function that takes the 'nested' list and returns a
//       description of what was expected ("x or y or z")
//   - matcher: function that takes a state and continuation and attempts to
//       match
function new_parser($kind, $options = []) {
    $nested = isset($options['nested']) ? $options['nested'] : [];
    $describer = isset($options['describer']) ? $options['describer'] : $kind;
    $matcher = isset($options['matcher'])
        ? $options['matcher']
        : ($state, $cont) ==> { throw new \Exception('Undefined matcher'); };

    if (isset($options['wrap']) && $options['wrap']) {
        $nested = [$options['wrap']];
        $describer = $ps ==> implode('', $ps);
    }

    return new Parser($kind, $nested, $describer, $matcher);
}

class Parser {
    public $kind, $nested, $describer, $matcher;
    public $recursing = false;
    function __construct($kind, $nested, $describer, $matcher) {
        $this->kind = $kind;
        $this->nested = $nested;
        $this->describer = $describer;
        $this->matcher = $matcher;
    }
    function nested_list() {
        if ($this->recursing) {
            return '...';
        }
        if (count($nested) === 0) {
            return '';
        }
        $this->recursing = true;
        $rv = implode(', ', array_map($p ==> resolve($p)->nested_list(), $this->nested));
        $this->recursing = false;
        return "($rv)";
    }
    function description() {
        if ($this->recursing) {
            return '...';
        }
        if (is_string($this->describer)) {
            return $this->describer;
        }
        $this->recursing = true;
        $ps = array_map($p ==> resolve($p)->description(), $this->nested);
        $this->recursing = false;
        $this->describer = call_user_func($this->describer, $ps);
        return $this->describer;
    }
    function __toString() {
        $id = spl_object_hash($this);
        return "Parser[$id, {$this->kind}]".$this->nested_list();
    }
    // helper for internal use: immediately fail.
    function fail($state, $cont) {
        return $cont(new NoMatch($state, 'Expected '.$this->description()));
    }
    // executes @matcher, passing the result (Match or NoMatch) to 'cont'.
    function parse($state, $cont) {
        $state = $state->deeper($this);
        $entry = $state->get_cache($this);
        if (count($entry->continuations) === 0) {
            // first to try it!
            $entry->continuations[] = $cont;
            $state->add_job(
                () ==> "parse $state, {$this}",
                () ==> $matcher($state, $rv ==> {
                    // push our (new?) result
                    if (!in_array($rv, $entry->results)) {
                        $entry->results[] = $rv;
                        foreach ($entry->continuations as $c) {
                            $c($rv);
                        }
                    }
                })
            );
        } else {
            $entry->continuations[] = $cont;
            foreach ($entry->results as $r) {
                $cont($r);
            }
        }
    }
    // consume an entire text with this parser. convert failure into an exception.
    function run($text, $options) {
        $rv = consume($this, $text, $options);
        if (!$rv->ok) {
            $e = new Exception($rv->message);
            $e->state = $rv->state;
            throw $e;
        }
        return $rv->match;
    }

    // ----- transformations and combinations:

    // transforms the error message of a parser
    function on_fail($new_message) {
        return new_parser('on_fail', [
            'wrap' => $this,
            'matcher' => ($state, $cont) ==>
                $this->parse($state, $rv ==> {
                    if ($rv->ok || $rv->abort) {
                        return $cont($rv);
                    }
                    return $cont(new NoMatch($rv->state, $new_message, $rv->abort));
                }),
        ]);
    }
    // transforms the result of a parser if it succeeds.
    function on_match($f) {
        return new_parser('on_match', [
            'wrap' => $this,
            'matcher' => ($state, $cont) ==>
                $this->parse($state, $rv ==> {
                    if (!$rv->ok) {
                        return $cont($rv);
                    }
                    if (is_callable($f)) {
                        try {
                            $result = $f($rv->match, $rv->state->flip());
                            if ($result instanceof Parser) {
                                return $result->parse($rv->state, $cont);
                            }
                            return $cont(new Match($rv->state, $result, $rv->commit));
                        } catch (Exception $e) {
                            return $cont(new NoMatch($rv->state, (string) $e, $rv->commit));
                        }
                    }
                    return $cont(new Match($rv->state, $f, $rv->commit));
                });
        ]);
    }
    // only succeed if f(match) returns true.
    function match_if($f) {
        return new_parser('match_if', [
            'wrap' => $this,
            'matcher' => ($state, $cont) ==>
                $this->parse($state, $rv ==> {
                    if (!$rv->ok) {
                        return $cont($rv);
                    }
                    if (!$f($rv->match)) {
                        return $cont(new NoMatch($state, 'Expected '.$this->description(), $rv->commit));
                    }
                    return $cont($rv);
                }),
        ]);
    }
    function describe($message) {
        $this->describer = () ==> $message;
        return $this->on_fail("Expected $message");
    }

    // ----- convenience methods for accessing the combinators

    function then($p) {
        return combiners\chain($this, $p, ($a, $b) ==> [$a, $b]);
    }

    function or(/** $others... */) {
        $others = func_get_args();
        return call_user_func_array('combiners\alt', $this, $others);
    }
    function optional($default_value = '') {
        return combiners\optional($this, $default_value);
    }
    function check() {
        return combiners\check($this);
    }
    function commit() {
        return combiners\commit($this);
    }
    function not() {
        return combiners\not($this);
    }
    function drop() {
        return combiners\drop($this);
    }
    function repeat($min_count, $max_count) {
        return combiners\repeat($this, $min_count, $max_count);
    }
    function times($count) {
        return combiners\repeat($this, $count, $count);
    }
}

class EndParser extends Parser {
    function __construct() {
        $matcher = ($state, $cont) ==>
            ($state->loc->pos === $state->internal->end)
                ? $cont(new Match($state, null, false))
                // what is $this?
                : $this->fail($state, $cont);
        parent::__construct('end', [], 'end', $matcher);
    }
}

// matches the end of the string.
function end() {
    return new EndParser();
}

class RejectParser {
    function __construct() {
        $matcher = ($state, $cont) ==>
            $cont(NoMatch($state, 'failure'));
        parent::__construct('reject', [], 'reject', $matcher);
    }
}

// never matches anything.
function reject() {
    return new RejectParser();
}

__HALT_COMPILER();

# always matches without consuming input and yields the given value.
succeed = (v) ->
  newParser "succeed",
    matcher: (state, cont) ->
      cont(new Match(state, v, false))

# matches a literal string.
string = (s) ->
  len = s.length
  newParser "lit: '#{s}'",
    describer: "'#{s}'"
    matcher: (state, cont) ->
      candidate = state.internal.text.slice(state.loc.pos, state.loc.pos + len)
      if candidate == s
        cont(new Match(state.advance(len), candidate, false))
      else
        @fail(state, cont)

# matches a regex
regex = (r) ->
  i = if r.ignoreCase then "i" else ""
  m = if r.multiline then "m" else ""
  source = if r.source[0] == "^" then r.source else ("^" + r.source)
  r2 = new RegExp(source, i + m)
  newParser "re: #{r.toString()}",
    describer: r.toString()
    matcher: (state, cont) ->
      m = r2.exec(state.internal.text.slice(state.loc.pos))
      if m? then cont(new Match(state.advance(m[0].length), m, false)) else @fail(state, cont)

# ----- top-level API:

# execute a parser over a string.
parse = (p, str, options = {}) ->
  _count = 0
  state = if str instanceof ParserState then str else new ParserState(str)
  state.stateName = "start"
  if options.debugGraph then state.startDebugGraph()
  p = resolve(p)
  successes = []
  failures = []
  state.addJob (=> "start: #{state}, #{p.toString()}"), ->
    p.parse state, (rv) ->
      if rv.ok
        rv.state.logSuccess()
        successes.push rv
      else
        rv.state.logFailure()
        failures.push rv
  while state.internal.trampoline.ready() and successes.length == 0
    state.internal.trampoline.next()
  # message with 'abort' set has highest priority. secondary sort by index.
  failures.sort (a, b) ->
    if a.abort != b.abort
      if b.abort then 1 else -1
    else
      b.state.depth - a.state.depth
  if successes.length > 0
    successes[0]
  else
    failures[0]

# must match the entire string, to the end.
consume = (p, str, options) ->
  p = combiners.chain implicit(p), end, (a, b) -> a
  parse(p, str, options)


exports.newParser = newParser
exports.Parser = Parser

exports.end = end
exports.reject = reject
exports.succeed = succeed
exports.string = string
exports.regex = regex

exports.parse = parse
exports.consume = consume
