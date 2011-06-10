public class Test2 {
    int method1 (int x) {
	int a;

	switch (x) {
	case 0:
	case 1:
	    a = x;
	    break;
	case 5:
	    a = 8;
	    break;
	default:
	    a = this.method1(x-1) + this.method1(x-2);
	    break;
	}

	return a;
    }

    String method2 (int x) {
	String s = "s";
	switch (x) {
	case 0:
	    s = s + x;
	    break;
	case 1:
	    s = s + x + x;
	    break;
	case 2:
	    s = s + x + x + x;
	    break;
	case 3:
	    s = s + x + x + x + x;
	    break;
	default:
	    s = "default";
	    break;
	}
	return s;
    }
}