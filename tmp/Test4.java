public class Test4 {
    private int x_ = 0;
    private int y_ = 0;

    public Test4 () {
    }

    public Test4 (int x) {
	x_ = x;
    }

    public Test4 (int x, int y) {
	x_ = x;
	y_ = y;
    }

    public int getsum () {
	return x_ + y_;
    }

    public static void main (String[] args) {
	Test4 a = new Test4();
	Test4 b = new Test4(1);
	Test4 c = new Test4(2, 3);

	System.out.println(a.getsum());
	System.out.println(b.getsum());
	System.out.println(c.getsum());
    }
}