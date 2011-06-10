class Test6 {
    public final int x = 2;
    public final int y = 65536;
    public final String s = "hello";
    public final float a = 3.14F;
    public final double b = 3.1415926358979;

    public void doit () {
	System.out.println(x);
	System.out.println(y);
	System.out.println(s);
	System.out.println(a);
	System.out.println(b);
    }

    public static void main (String[] args) {
	Test6 a = new Test6();
	a.doit();
    }
}
