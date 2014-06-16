<?hh

namespace igorw\gll;

// The heart of the parsing mechanism.  Contains the trampoline structure,
// the parsing dispatch function, the nodes where listeners are stored,
// the different types of listeners, and the loop for executing the various
// listeners and parse commands that are on the stack.

function get_parser($grammar, $p) {
  return isset($grammar[$p]) ? $grammar[$p] : $p;
}

function parse($parser, $index, $tramp) {
    $parsers = [
        'nt'        => 'igorw\gll\non_terminal_parse',
        'alt'       => 'igorw\gll\alt_parse',
        'cat'       => 'igorw\gll\cat_parse',
        'string'    => 'igorw\gll\string_parse',
        'string_ci' => 'igorw\gll\string_case_insensitive_parse',
        'epsilon'   => 'igorw\gll\epsilon_parse',
        'opt'       => 'igorw\gll\opt_parse',
        'plus'      => 'igorw\gll\plus_parse',
        'rep'       => 'igorw\gll\rep_parse',
        'star'      => 'igorw\gll\star_parse',
        'regexp'    => 'igorw\gll\regexp_parse',
        'look'      => 'igorw\gll\lookahead_parse',
        'neg'       => 'igorw\gll\negative_lookahead_parse',
        'ord'       => 'igorw\gll\ordered_alt_parse',
    ];

    return $parsers[$parser['tag']]($parser, $index, $tramp);
}

function full_parse($parser, $index, $tramp) {
    $parsers = [
        'nt'        => 'igorw\gll\non_terminal_full_parse',
        'alt'       => 'igorw\gll\alt_full_parse',
        'cat'       => 'igorw\gll\cat_full_parse',
        'string'    => 'igorw\gll\string_full_parse',
        'string_ci' => 'igorw\gll\string_case_insensitive_full_parse',
        'epsilon'   => 'igorw\gll\epsilon_full_parse',
        'opt'       => 'igorw\gll\opt_full_parse',
        'plus'      => 'igorw\gll\plus_full_parse',
        'rep'       => 'igorw\gll\rep_full_parse',
        'star'      => 'igorw\gll\star_full_parse',
        'regexp'    => 'igorw\gll\regexp_full_parse',
        'look'      => 'igorw\gll\lookahead_full_parse',
        'neg'       => 'igorw\gll\negative_lookahead_full_parse',
        'ord'       => 'igorw\gll\ordered_alt_full_parse',
    ];

    return $parsers[$parser['tag']]($parser, $index, $tramp);
}

class Failure {
    public $index;
    public $reason;
    function __construct($index, $reason) {
        $this->index = $index;
        $this->reason = $reason;
    }
}

// The trampoline structure contains the grammar, text to parse, a stack and a nodes
// Also contains an atom to hold successes and one to hold index of failure point.
// grammar is a map from non-terminals to parsers
// text is a string
// stack is an atom of a vector containing items implementing the Execute protocol.
// nodes is an atom containing a map from [index parser] pairs to Nodes
// success contains a successful parse
// failure contains the index of the furthest-along failure

class Tramp {
    public $grammar, $text, $fail_index, $node_builder,
           $stack, $next_stack, $generation, $negative_listeners,
           $msg_cache, $nodes, $success, $failure;
    function __construct(
        $grammar, $text, $fail_index = -1, $node_builder = null,
        $stack = null, $next_stack = null, $generation = null, $negative_listeners = null,
        $msg_cache = null, $nodes = null, $success = null, $failure = null
    ) {
        $this->grammar = $grammer;
        $this->text = $text;
        $this->fail_index = $fail_index;
        $this->node_builder = $node_builder;
        $this->stack = $stack ?: new SplStack();
        $this->next_stack = $next_stack ?: new SplStack();
        // todo make mutable
        $this->generation = $generation ?: 0;
        $this->negative_listeners = $negative_listeners ?: new SplStack();
        // maps below, also make mutable
        $this->msg_cache = $msg_cache ?: [];
        $this->nodes = $nodes ?: [];
        $this->success = $success ?: null;
        $this->failure = $failure ?: new Failure(0, []);
    }
}

function make_tramp($grammar, $text, $fail_index = -1, $node_builder = null) {
    return new Tramp($grammar, $text, $fail_index, $node_builder);
}

// A Success record contains the result and the index to continue from
function make_success($result, $index) {
    return ['result' => $result, 'index' => $index];
}

function total_success($tramp, $succ) {
    return count($tramp['text']) === $succ['index'];
}

// The trampoline's nodes field is map from [index parser] pairs to Nodes
// Nodes track the results of a given parser at a given index, and the listeners
// who care about the result.
// results are expected to be refs of sets.
// listeners are refs of vectors.

class Node {
    public $listeners, $full_listeners, $results, $full_results;
    public function __construct($listeners, $full_listeners, $results, $full_results) {
        $this->listeners = $listeners;
        $this->full_listeners = $full_listeners;
        $this->results = $results;
        $this->full_results = $full_results;
    }
}

function make_node() {
    // todo make mutable
    // [list list set set]
    return new Node([], [], [], []);
}

// Trampoline helper functions

// Pushes an item onto the trampoline's stack
function push_stack($tramp, $item) {
    $tramp->stack->push($item);
}

// Pushes onto stack a message to a given listener about a result
function push_message($tramp, $listener, $result) {
    $cache = $tramp->msg_cache;
    $i = $result['index'];
    $k = [$listener, $i]; // spl_object_hash?
    $c = $cache[$k] ?: 0; // force($cache)
    $f = () ==> $listener($result);
    if ($c > $tramp->generation) { // force($tramp->generation)
        $tramp->next_stack->push($f);
    } else {
        $tramp->stack->push($f);
    }
    $cache[$k] = $c + 1;
}

// Tests whether node already has a listener
function listener_exists($tramp, $node_key) {
    $nodes = $tramp->nodes;
    if (!isset($nodes[$node_key])) {
        return false;
    }
    $node = $nodes[$node_key]; // force($nodes)
    return count($node->listeners) > 0;
}

// Tests whether node already has a listener or full-listener
function full_listener_exists($tramp, $node_key) {
    $nodes = $tramp->nodes;
    if (!isset($nodes[$node_key])) {
        return false;
    }
    $node = $nodes[$node_key];
    return count($node->full_listeners) > 0 ||
           count($node->listeners) > 0;
}

// Tests whether node has a result or full-result
function result_exists($tramp, $node_key) {
    $nodes = $tramp->nodes;
    if (!isset($nodes[$node_key])) {
        return false;
    }
    return count($node->full_results) > 0 ||
           count($node->results) > 0;
}

// Tests whether node has a full-result
function full_result_exists($tramp, $node_key) {
    $nodes = $tramp->nodes;
    if (!isset($nodes[$node_key])) {
        return false;
    }
    $node = $nodes[$node_key];
    return count($node->full_results) > 0;
}

// Gets node if already exists, otherwise creates one
function node_get($tramp, $node_key) {
    $nodes = $tramp->nodes;
    if (isset($nodes[$node_key])) {
        return $nodes[$node_key];
    }
    $node = make_node();
    $nodes[$node_key] = $node;
    return $node;
}

__HALT_COMPILER();

(defn push-result
  "Pushes a result into the trampoline's node.
   Categorizes as either result or full-result.
   Schedules notification to all existing listeners of result
   (Full listeners only get notified about full results)"
  [tramp node-key result]
  (dprintln "Push result" (node-key 0) (:tag (node-key 1)) result)
  (let [node (node-get tramp node-key)
        parser (node-key 1)
        ;; reduce result with reduction function if it exists
        result (if (:hide parser)
                 (assoc result :result nil)
                 result)
        result (if-let [reduction-function (:red parser)]
                 (make-success
                   (safe-with-meta
                     (red/apply-reduction reduction-function (:result result))
                     {::start-index (node-key 0) ::end-index (:index result)})
                   (:index result))
                 result)
        total? (total-success? tramp result)
        results (if total? (:full-results node) (:results node))]
    (when (not (@results result))  ; when result is not already in @results
      (debug (add! :push-result))
      (swap! results conj result)
      (doseq [listener @(:listeners node)]
        (push-message tramp listener result))
      (when total?
        (doseq [listener @(:full-listeners node)]
          (push-message tramp listener result))))))

(defn push-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing results.
   Initiates parse if necessary"
  [tramp node-key listener]
  (dprintln "push-listener" [(node-key 1) (node-key 0)] (type listener))
  (let [listener-already-exists? (listener-exists? tramp node-key)
        node (node-get tramp node-key)
        listeners (:listeners node)]
    (debug (add! :push-listener))
    (swap! listeners conj listener)
    (doseq [result @(:results node)]
      (push-message tramp listener result))
    (doseq [result @(:full-results node)]
      (push-message tramp listener result))
    (when (not listener-already-exists?)
      (push-stack tramp #(-parse (node-key 1) (node-key 0) tramp)))))

(defn push-full-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing full results."
  [tramp node-key listener]
  (let [full-listener-already-exists? (full-listener-exists? tramp node-key)
        node (node-get tramp node-key)
        listeners (:full-listeners node)]
    (debug (add! :push-full-listener))
    (swap! listeners conj listener)
    (doseq [result @(:full-results node)]
      (push-message tramp listener result))
    (when (not full-listener-already-exists?)
      (push-stack tramp #(-full-parse (node-key 1) (node-key 0) tramp)))))

(defn push-negative-listener
  "Pushes a thunk onto the trampoline's negative-listener stack."
  [tramp negative-listener]
  (swap! (:negative-listeners tramp) conj negative-listener))

;(defn success [tramp node-key result end]
;  (push-result tramp node-key (make-success result end)))

(defmacro success [tramp node-key result end]
  `(push-result ~tramp ~node-key (make-success ~result ~end)))

(declare build-node-with-meta)
(defn fail [tramp node-key index reason]
  (swap! (:failure tramp)
         (fn [failure]
           (let [current-index (:index failure)]
             (case (compare index current-index)
               1 (Failure. index [reason])
               0 (Failure. index (conj (:reason failure) reason))
               -1  failure))))
  #_(dprintln "Fail index" (:fail-index tramp))
  (when (= index (:fail-index tramp))
    (success tramp node-key
             (build-node-with-meta
               (:node-builder tramp) :instaparse/failure (subs (:text tramp) index)
               index (count (:text tramp)))
             (count (:text tramp)))))

;; Stack helper functions

(defn step
  "Executes one thing on the stack (not threadsafe)"
  [stack]
  (let [top (peek @stack)]
    (swap! stack pop)
    #_(dprintln "Top" top (meta top))
    (top)))

(defn run
  "Executes the stack until exhausted"
  ([tramp] (run tramp nil))
  ([tramp found-result?]
    (let [stack (:stack tramp)]
      ;_ (dprintln found-result? (count @(:stack tramp)) (count @(:next-stack tramp)))
      (cond
        @(:success tramp)
        (lazy-seq (cons (:result @(:success tramp))
                        (do (reset! (:success tramp) nil)
                          (run tramp true))))

        (pos? (count @stack))
        (do ;(dprintln "stacks" (count @stack) (count @(:next-stack tramp)))
          (step stack) (recur tramp found-result?))

        (pos? (count @(:negative-listeners tramp)))
        (let [listener (peek @(:negative-listeners tramp))]
          (listener)
          (swap! (:negative-listeners tramp) pop)
          (recur tramp found-result?))

        found-result?
        (let [next-stack (:next-stack tramp)]
          (dprintln "Swapping stacks" (count @(:stack tramp))
                   (count @(:next-stack tramp)))
          (reset! stack @next-stack)
          (reset! next-stack [])
          (swap! (:generation tramp) inc)
          (dprintln "Swapped stacks" (count @(:stack tramp))
                   (count @(:next-stack tramp)))
          (recur tramp nil))

        :else nil))))

;; Listeners

; There are six kinds of listeners that receive notifications
; The first kind is a NodeListener which simply listens for a completed parse result
; Takes the node-key of the parser which is awaiting this result.

(defn NodeListener [node-key tramp]
  (fn [result]
    (dprintln "Node Listener received" [(node-key 0) (:tag (node-key 1))] "result" result)
    (push-result tramp node-key result)))

; The second kind of listener handles lookahead.
(defn LookListener [node-key tramp]
  (fn [result]
    (success tramp node-key nil (node-key 0))))

; The third kind of listener is a CatListener which listens at each stage of the
; concatenation parser to carry on the next step.  Think of it as a parse continuation.
; A CatListener needs to know the sequence of results for the parsers that have come
; before, and a list of parsers that remain.  Also, the node-key of the final node
; that needs to know the overall result of the cat parser.

(defn CatListener [results-so-far parser-sequence node-key tramp]
  (dpprint {:tag :CatListener
           :results-so-far results-so-far
           :parser-sequence (map :tag parser-sequence)
           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result]
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (afs/conj-flat results-so-far parsed-result)]
      (if (seq parser-sequence)
        (push-listener tramp [continue-index (first parser-sequence)]
                       (CatListener new-results-so-far (next parser-sequence) node-key tramp))
        (success tramp node-key new-results-so-far continue-index)))))

(defn CatFullListener [results-so-far parser-sequence node-key tramp]
;  (dpprint {:tag :CatFullListener
;           :results-so-far results-so-far
;           :parser-sequence (map :tag parser-sequence)
;           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result]
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (afs/conj-flat results-so-far parsed-result)]
      (cond
        (red/singleton? parser-sequence)
        (push-full-listener tramp [continue-index (first parser-sequence)]
                            (CatFullListener new-results-so-far (next parser-sequence) node-key tramp))

        (seq parser-sequence)
        (push-listener tramp [continue-index (first parser-sequence)]
                       (CatFullListener new-results-so-far (next parser-sequence) node-key tramp))

        :else
        (success tramp node-key new-results-so-far continue-index)))))

; The fourth kind of listener is a PlusListener, which is a variation of
; the CatListener but optimized for "one or more" parsers.

(defn PlusListener [results-so-far parser prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      (if (= continue-index prev-index)
        (when (zero? (count results-so-far))
          (success tramp node-key nil continue-index))
        (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
          (push-listener tramp [continue-index parser]
                         (PlusListener new-results-so-far parser continue-index
                                       node-key tramp))
          (success tramp node-key new-results-so-far continue-index))))))

(defn PlusFullListener [results-so-far parser prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      (if (= continue-index prev-index)
        (when (zero? (count results-so-far))
          (success tramp node-key nil continue-index))
        (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
          (if (= continue-index (count (:text tramp)))
            (success tramp node-key new-results-so-far continue-index)
            (push-listener tramp [continue-index parser]
                           (PlusFullListener new-results-so-far parser continue-index
                                             node-key tramp))))))))

; The fifth kind of listener is a RepListener, which wants between m and n repetitions of a parser

(defn RepListener [results-so-far parser m n prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      ;(dprintln "Rep" (type results-so-far))
      (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
        (when (<= m (count new-results-so-far) n)
          (success tramp node-key new-results-so-far continue-index))
        (when (< (count new-results-so-far) n)
          (push-listener tramp [continue-index parser]
                         (RepListener new-results-so-far parser m n continue-index
                                       node-key tramp)))))))

(defn RepFullListener [results-so-far parser m n prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      ;(dprintln "RepFull" (type parsed-result))
      (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
        (if (= continue-index (count (:text tramp)))
          (when (<= m (count new-results-so-far) n)
            (success tramp node-key new-results-so-far continue-index))
          (when (< (count new-results-so-far) n)
            (push-listener tramp [continue-index parser]
                           (RepFullListener new-results-so-far parser m n continue-index
                                             node-key tramp))))))))

; The top level listener is the final kind of listener

(defn TopListener [tramp]
  (fn [result]
    (reset! (:success tramp) result)))

;; Parsers

(defn string-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (subs text index end)]
    (if (= string head)
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string}))))

(defn string-full-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (subs text index end)]
    (if (and (= end (count text)) (= string head))
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string :full true}))))

(defn string-case-insensitive-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (subs text index end)]
    (if (.equalsIgnoreCase ^String string head)
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string}))))

(defn string-case-insensitive-full-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (subs text index end)]
    (if (and (= end (count text)) (.equalsIgnoreCase ^String string head))
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string :full true}))))

(defn re-match-at-front [regexp text]
  (let [^java.util.regex.Matcher matcher (re-matcher regexp text)
        match? (.lookingAt matcher)]
    (when match?
      (.group matcher))))

(defn regexp-parse
  [this index tramp]
  (let [regexp (:regexp this)
        ^Segment text (:segment tramp)
        substring (.subSequence text index (.length text))
        match (re-match-at-front regexp substring)]
    (if match
      (success tramp [index this] match (+ index (count match)))
      (fail tramp [index this] index
            {:tag :regexp :expecting regexp}))))

(defn regexp-full-parse
  [this index tramp]
  (let [regexp (:regexp this)
        ^Segment text (:segment tramp)
        substring (.subSequence text index (.length text))
        match (re-match-at-front regexp substring)
        desired-length (- (count text) index)]
    (if (and match (= (count match) desired-length))
      (success tramp [index this] match (count text)))
      (fail tramp [index this] index
            {:tag :regexp :expecting regexp :full true})))

(let [empty-cat-result afs/EMPTY]
    (defn cat-parse
      [this index tramp]
      (let [parsers (:parsers this)]
        ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
        ; and with a final target of notifying this parser when the whole sequence is complete
        (push-listener tramp [index (first parsers)]
                    (CatListener empty-cat-result (next parsers) [index this] tramp))))

    (defn cat-full-parse
      [this index tramp]
      (let [parsers (:parsers this)]
        ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
        ; and with a final target of notifying this parser when the whole sequence is complete
        (push-listener tramp [index (first parsers)]
                    (CatFullListener empty-cat-result (next parsers) [index this] tramp))))

 (defn plus-parse
      [this index tramp]
      (let [parser (:parser this)]
        (push-listener tramp [index parser]
                    (PlusListener empty-cat-result parser index [index this] tramp))))

 (defn plus-full-parse
   [this index tramp]
   (let [parser (:parser this)]
     (push-listener tramp [index parser]
                    (PlusFullListener empty-cat-result parser index [index this] tramp))))

 (defn rep-parse
   [this index tramp]
   (let [parser (:parser this),
         m (:min this),
         n (:max this)]
     (if (zero? m)
       (do
         (success tramp [index this] nil index)
         (when (>= n 1)
           (push-listener tramp [index parser]
                          (RepListener empty-cat-result parser 1 n index [index this] tramp))))
       (push-listener tramp [index parser]
                      (RepListener empty-cat-result parser m n index [index this] tramp)))))

 (defn rep-full-parse
   [this index tramp]
   (let [parser (:parser this),
         m (:min this),
         n (:max this)]
     (if (zero? m)
       (do
         (success tramp [index this] nil index)
         (when (>= n 1)
           (push-listener tramp [index parser]
                          (RepFullListener empty-cat-result parser 1 n index [index this] tramp))))
       (push-listener tramp [index parser]
                      (RepFullListener empty-cat-result parser m n index [index this] tramp)))))

 (defn star-parse
      [this index tramp]
      (let [parser (:parser this)]
        (push-listener tramp [index parser]
                    (PlusListener empty-cat-result parser index [index this] tramp))
     (success tramp [index this] nil index)))

 (defn star-full-parse
   [this index tramp]
   (let [parser (:parser this)]
     (if (= index (count (:text tramp)))
       (success tramp [index this] nil index)
       (do
         (push-listener tramp [index parser]
                        (PlusFullListener empty-cat-result parser index [index this] tramp))))))
 )

(defn alt-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    (doseq [parser parsers]
      (push-listener tramp [index parser] (NodeListener [index this] tramp)))))

(defn alt-full-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    (doseq [parser parsers]
      (push-full-listener tramp [index parser] (NodeListener [index this] tramp)))))

(defn ordered-alt-parse
  [this index tramp]
  (let [parser1 (:parser1 this)
        parser2 (:parser2 this)
        node-key-parser1 [index parser1]
        node-key-parser2 [index parser2]
        listener (NodeListener [index this] tramp)]
    (push-listener tramp node-key-parser1 listener)
    (push-negative-listener
      tramp
      #(push-listener tramp node-key-parser2 listener))))

(defn ordered-alt-full-parse
  [this index tramp]
  (let [parser1 (:parser1 this)
        parser2 (:parser2 this)
        node-key-parser1 [index parser1]
        node-key-parser2 [index parser2]
        listener (NodeListener [index this] tramp)]
    (push-full-listener tramp node-key-parser1 listener)
    (push-negative-listener
      tramp
      #(push-full-listener tramp node-key-parser2 listener))))

(defn opt-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] (NodeListener [index this] tramp))
    (success tramp [index this] nil index)))

(defn opt-full-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-full-listener tramp [index parser] (NodeListener [index this] tramp))
    (if (= index (count (:text tramp)))
      (success tramp [index this] nil index)
      (fail tramp [index this] index {:tag :optional :expecting :end-of-string}))))

(defn non-terminal-parse
  [this index tramp]
  (let [parser (get-parser (:grammar tramp) (:keyword this))]
    (push-listener tramp [index parser] (NodeListener [index this] tramp))))

(defn non-terminal-full-parse
  [this index tramp]
  (let [parser (get-parser (:grammar tramp) (:keyword this))]
    (push-full-listener tramp [index parser] (NodeListener [index this] tramp))))

(defn lookahead-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] (LookListener [index this] tramp))))

(defn lookahead-full-parse
  [this index tramp]
  (if (= index (count (:text tramp)))
    (lookahead-parse this index tramp)
    (fail tramp [index this] index {:tag :lookahead :expecting :end-of-string})))

;(declare negative-parse?)
;(defn negative-lookahead-parse
;  [this index tramp]
;  (let [parser (:parser this)
;        remaining-text (subs (:text tramp) index)]
;    (if (negative-parse? (:grammar tramp) parser remaining-text)
;      (success tramp [index this] nil index)
;      (fail tramp index :negative-lookahead))))

(defn negative-lookahead-parse
  [this index tramp]
  (let [parser (:parser this)
        node-key [index parser]]
    (if (result-exists? tramp node-key)
      (fail tramp [index this] index {:tag :negative-lookahead})
      (do
        (push-listener tramp node-key
                       (let [fail-send (delay (fail tramp [index this] index
                                                    {:tag :negative-lookahead
                                                     :expecting {:NOT
                                                                 (print/combinators->str parser)}}))]
                         (fn [result] (force fail-send))))
        (push-negative-listener
          tramp
          #(when (not (result-exists? tramp node-key))
             (success tramp [index this] nil index)))))))

(defn epsilon-parse
  [this index tramp] (success tramp [index this] nil index))
(defn epsilon-full-parse
  [this index tramp]
  (if (= index (count (:text tramp)))
    (success tramp [index this] nil index)
    (fail tramp [index this] index {:tag :Epsilon :expecting :end-of-string})))

;; Parsing functions

(defn start-parser [tramp parser partial?]
  (if partial?
    (push-listener tramp [0 parser] (TopListener tramp))
    (push-full-listener tramp [0 parser] (TopListener tramp))))

(defn parses [grammar start text partial?]
  (debug (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      all-parses
      (with-meta ()
        (fail/augment-failure @(:failure tramp) text)))))

(defn parse [grammar start text partial?]
  (debug (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      (first all-parses)
      (fail/augment-failure @(:failure tramp) text))))

;; The node builder function is what we use to build the failure nodes
;; but we want to include start and end metadata as well.

(defn build-node-with-meta [node-builder tag content start end]
  (with-meta
    (node-builder tag content)
    {::start-index start ::end-index end}))

(defn build-total-failure-node [node-builder start text]
  (let [build-failure-node
        (build-node-with-meta node-builder :instaparse/failure text 0 (count text)),
        build-start-node
        (build-node-with-meta node-builder start build-failure-node 0 (count text))]
    build-start-node))

(defn parses-total-after-fail
  [grammar start text fail-index partial? node-builder]
  (dprintln "Parses-total-after-fail")
  (let [tramp (make-tramp grammar text fail-index node-builder)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      all-parses
      (list (build-total-failure-node node-builder start text)))))

(defn merge-meta
  "A variation on with-meta that merges the existing metamap into the new metamap,
rather than overwriting the metamap entirely."
  [obj metamap]
  (with-meta obj (merge metamap (meta obj))))

(defn parses-total
  [grammar start text partial? node-builder]
  (debug (clear!))
  (let [all-parses (parses grammar start text partial?)]
    (if (seq all-parses)
      all-parses
      (merge-meta
        (parses-total-after-fail grammar start text
                                 (:index (meta all-parses))
                                 partial? node-builder)
        (meta all-parses)))))

(defn parse-total-after-fail
  [grammar start text fail-index partial? node-builder]
  (dprintln "Parse-total-after-fail")
  (let [tramp (make-tramp grammar text fail-index node-builder)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      (first all-parses)
      (build-total-failure-node node-builder start text))))

(defn parse-total
  [grammar start text partial? node-builder]
  (debug (clear!))
  (let [result (parse grammar start text partial?)]
    (if-not (instance? Failure result)
      result
      (merge-meta
        (parse-total-after-fail grammar start text
                                (:index result)
                                partial? node-builder)
        result))))

;; Variation, but not for end-user

;(defn negative-parse?
;  "takes pre-processed grammar and parser"
;  [grammar parser text]
;  (let [tramp (make-tramp grammar text)]
;    (push-listener tramp [0 parser] (TopListener tramp))
;    (empty? (run tramp))))
;