<?hh

// mathematical expression example
// taken from: https://github.com/epsil/gll

namespace igorw\smaug;

require __DIR__.'/../src/parser.php';

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

// var_dump(iterator_to_array(run_parser($num, '1')));
// var_dump(iterator_to_array(run_parser($num, '42')));

// var_dump(iterator_to_array(run_parser($factor, '42')));
// var_dump(iterator_to_array(run_parser($factor, '(42)')));

// var_dump(iterator_to_array(run_parser($expr, '42')));
// var_dump(iterator_to_array(run_parser($expr, '1*2+3*4')));
// var_dump(iterator_to_array(run_parser($expr, '9-(5+2)')));
