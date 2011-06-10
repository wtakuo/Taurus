class Test5 {
    int method1 (int i) {
	try {
	    if (i == 0) return i;
	    if (i == 1) return i-1;
	    if (i == 2) return i-2;
	    if (i == 3) return i-3;
	    return i;
	} finally {
	    i = i + 1;
	}
    }
}