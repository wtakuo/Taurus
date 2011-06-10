
class MailerPolicyObserver {
    private StateKeeper state = new StateKeeper();

    private static MailerPolicyObserver inst = null;

    public static MailerPolicyObserver getObserver () {
        if (inst == null) {
            inst = new MailerPolicyObserver ();
        }
        return inst;
    }

    // Mailer.getUserName()
    void observe_getUserName (Mailer mailer) throws Exception
    {
        System.out.println("getUserName()");
        switch (state.get()) {
        case 0: 
            state.set(1);
            break;
        default:
            break;
        }
    }

    // Mailer.doQuery(Action)
    void observe_doQuery (Mailer mailer, Action action) 
    	throws Exception
    {
        switch (state.get()) {
        case 0:
            state.set(1);
            break;
        case 2:
            throw new Exception();
        }
    }

    void observe_getCalendar (Mailer mailer) throws Exception
    {
        switch (state.get()) {
        case 0:
        case 1:
            state.set(2);
        }
    }

    // Mailer.sendMail(String, String, String)
    void observe_sendMail (Mailer mailer, 
                           String toAddr, 
                           String subject, 
                           String body) 
        throws Exception
    {
        // System.out.println("sendMail (" + toAddr + ", " + subject + ", " +
        // 		   body + ")");
        switch (state.get()) {
        case 0:
            System.out.println("Policy violation: A query should be done before mailing.");
            throw new Exception();
        case 1:
            state.set(0);
            break;
        }
    }
}
