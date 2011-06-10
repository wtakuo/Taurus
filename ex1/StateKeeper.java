
import java.util.HashMap;

class StateKeeper {
    private HashMap h = new HashMap();

    public synchronized void set (int s) {
        Thread th = Thread.currentThread();
        h.put(th, new Integer(s));
    }

    public synchronized int get () {
        Thread th = Thread.currentThread();
        if (h.get(th) == null) {
            h.put(th, new Integer(0));
        }
        return ((Integer)h.get(th)).intValue();
    }
}