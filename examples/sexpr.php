<?hh

// wooh! s-expressions!

namespace igorw\smaug;

require __DIR__.'/../src/parser.php';

$p = new \ArrayObject();

$p['expr'] = delay_parser(() ==>
    alt($p['symbol'], $p['list'])
);

$p['symbol'] = regexp('\w+');

$p['list'] = delay_parser(() ==>
    alt(
        red(string('()'), () ==> []),
        red(seq(string('('), $p['members'], string(')')),
            ($l, $m, $r) ==> $m),
    )
);

$p['members'] = delay_parser(() ==>
    alt(
        red($p['expr'], $e ==> [$e]),
        red(seq($p['expr'], string(' '), $p['members']),
            ($e, $_, $m) ==> [$e, $m])
    )
);

var_dump(iterator_to_array(run_parser($p['expr'], 'foo')));
var_dump(iterator_to_array(run_parser($p['expr'], '()')));
var_dump(iterator_to_array(run_parser($p['expr'], '(foo)')));
var_dump(iterator_to_array(run_parser($p['expr'], '(foo bar)')));
var_dump(iterator_to_array(run_parser($p['expr'], '(foo bar (baz (qux quux the great)))')));
