class Test10 {
    public static void main (String[] args) {
	Test10Target a = new Test10Target();
	a.setObserver(new Test10Observer());
	a.doIt();
    }
}

