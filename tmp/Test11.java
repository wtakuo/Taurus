class Test11 {
    public static void main (String[] args) {
	Test11Target a = new Test11Target ();
	a.setObserver(new PrintObserver());
	a.doIt();
    }
}