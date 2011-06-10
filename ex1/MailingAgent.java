class MailingAgent implements MailApp {
    private Mailer mailer_;

    private String[] users = {
        "kyamada@psg.cs.titech.ac.jp",
        "tomo@psg.cs.titech.ac.jp",
        "akira@psg.cs.titech.ac.jp",
        "yonezawa@is.s.u-tokyo.ac.jp"
    };

    public void setMailer (Mailer mailer) {
        mailer_ = mailer; 
    }

    public String getFromAddr () throws Exception { 
        return "etsuya@anzen.is.titech.ac.jp"; 
    }

    public void run () throws Exception {
        String userName = mailer_.getUserName();
        String plan = mailer_.getCalendar().getToday();
        doMailing (userName, plan);
        mailer_.sendMail("akira@psg.cs.titech.ac.jp", "hi!", "....");
    }

    private void doMailing (final String userName, final String plan) 
        throws Exception {
        for (int i = 0; i < users.length; i++) {
            final int ii = i;
            mailer_.doQuery(new Action () {
                    public void doIt () {
                        mailer_.sendMail(users[ii],
                                         userName + "'s plan",
                                         plan);
                    }
                });
        }
    }
}