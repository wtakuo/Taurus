class Test10Target {
    int fact (int n) {
	if (n < 1)
	    return 1;
	else
	    return n * fact(n-1);
    }

    public void doIt () {
	for (int i=0; i < 10; i++) {
	    System.out.println("i!=" + fact(i));
	}
	System.err.println("hoge!");
    }
}

