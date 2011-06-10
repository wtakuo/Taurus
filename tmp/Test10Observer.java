
class Test10Observer {
    void observe_println(java.io.PrintStream t, java.lang.String a1) {
	System.out.println("target=" + t + ", arg=" + a1);
    }

    void observe_fact(Test10Target t, int a1) {
	System.out.println("target=" + t + ", arg=" + a1);
    }
}
