class Mailer {
    private String userName_;
    private String userAddr_;
    private String smtpHost_;
    private String pop3Host_;
    private Calendar calendar_;

    public Mailer (String userName, String userAddr, 
                   String smtpHost, String pop3Host) 
    {
        userName_ = userName;
        userAddr_ = userAddr;
        smtpHost_ = smtpHost;
        pop3Host_ = pop3Host;
        calendar_ = new Calendar();
    }

    public String getUserName () { 
        return userName_; 
    }

    public String getUserAddr () { 
        return userAddr_; 
    }

    public Calendar getCalendar () { 
        return calendar_;
    }

    public void sendMail (String toAddr, String subject, String body) {
        System.out.println("Sending an e-mail from " + userAddr_ + " to " +
                           toAddr + ".");
    }

    public void doQuery (Action action) {
        action.doIt();
    }
}

