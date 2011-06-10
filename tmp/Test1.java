public class Test1 {
    private int count = 0;
    public final static String name = "Test";
    public final static String version = "1.0";
    public int hoge = 71293;
    public long piyo = 330101201071L;
    public double pi = 3.14159265358979;

    int fact (int n) {
	count++;
	if (n == 0)
	    return 1;
	else
	    return n * this.fact(n-1);
    }

    public static void main (String[] args) throws Exception {
	Test1 test = new Test1 ();
	int n = 10;
	try {
	    System.out.println("fact(" + n + ") = " + test.fact(n));
	} catch (Exception e) {
	    System.err.println("exception");
	    throw e;
	}
	finally {
	    System.out.println("fin.");
	}
    }
}