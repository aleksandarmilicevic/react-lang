package react.verification;

import net.automatalib.util.automata.fsa.DFAs;
import net.automatalib.util.automata.Automata;
import net.automatalib.automata.fsa.impl.compact.CompactDFA;
import net.automatalib.words.Alphabet;
import net.automatalib.words.impl.Alphabets;
import net.automatalib.words.Word;
import net.automatalib.words.WordBuilder;

class StateStore {

    Alphabet<Integer> alpha;

    CompactDFA<Integer> states;

    public StateStore() {
        alpha = Alphabets.integers(0, 2);
        states = new CompactDFA(alpha);
    }

    public Alphabet<Integer> alphabet() {
        return alpha;
    }

    public int size() {
        return states.size();
    }

    public void minimize() {
        //Automata.invasiveMinimize<Integer, Integer, Integer, Boolean, Void, CompactDFA<Integer>>(states, alpha);
        Automata.invasiveMinimize(states, alpha);
    }

    public static Word<Integer> stateToWord(byte[] bytes) {
        WordBuilder<Integer> builder = new WordBuilder();

        for (int i = 0; i < bytes.length; i++) {
            byte b = bytes[i];
            if ((b & 0x1) != 0)  builder.add(1); else builder.add(0);
            if ((b & 0x2) != 0)  builder.add(1); else builder.add(0);
            if ((b & 0x4) != 0)  builder.add(1); else builder.add(0);
            if ((b & 0x8) != 0)  builder.add(1); else builder.add(0);
            if ((b & 0x10) != 0) builder.add(1); else builder.add(0);
            if ((b & 0x20) != 0) builder.add(1); else builder.add(0);
            if ((b & 0x40) != 0) builder.add(1); else builder.add(0);
            if ((b & 0x80) != 0) builder.add(1); else builder.add(0);
        }

        return builder.toWord();
    }
    
    public boolean contains(Word<Integer> w) {
        return states.computeOutput(w);
    }

    public boolean contains(byte[] state) {
        Word<Integer> w = stateToWord(state);
        return contains(w);
    }

    public void addDFA(CompactDFA<Integer> set) {
        states = DFAs.or(states, set, alpha);
    }

    public void addState(byte[] state) {
        addDFA(stateToDFA(state));
    }

    public CompactDFA<Integer> dfaFromWord(Word<Integer> w) {
        CompactDFA<Integer> dfa = new CompactDFA(alpha, w.length() + 2);
        int last;
        if (w.isEmpty()) {
            last = dfa.addInitialState(true);
        } else {
            last = dfa.addInitialState(false);
        }
        int err = dfa.addState(false);
        for (int idx = 0; idx < w.length() -1; idx++) {
            int sym = w.getSymbol(idx);
            int curr = dfa.addState(false);
            dfa.addTransition(last, sym, curr);
            int wrong;
            if (sym == 0) {
                wrong = 1;
            } else {
                wrong = 0;
            }
            dfa.addTransition(last, wrong, err);
            last = curr;
        }
        int curr = dfa.addState(true);
        dfa.addTransition(last, w.lastSymbol(), curr);
        dfa.addTransition(curr, 0, err);
        dfa.addTransition(curr, 1, err);
        return dfa;
    }

    public CompactDFA<Integer> stateToDFA(byte[] state) {
        return dfaFromWord(stateToWord(state));
    }
}
