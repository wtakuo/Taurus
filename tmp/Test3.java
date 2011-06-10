class Test3 {
    int fact (int n) {
	int a = 1;
	int k = 1;
	while (k <= n) {
	    a = a * k;
	    k = k + 1;
	}
	return a;
    }

    public static void main (String[] args) {
	System.out.println(new Test3().fact(10));
    }
}
