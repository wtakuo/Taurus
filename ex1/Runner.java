
class Runner {
    public static void main (String[] args) throws Exception {
        Mailer mailer = 
            new Mailer ("Takuo Watanabe",
                        "takuo@anzen.cs.titech.ac.jp",
                        "smtp.anzen.cs.titech.ac.jp",
                        "pop3.anzen.cs.titech.ac.jp");

        MailingAgent agt = new MailingAgent();
        agt.setMailer(mailer);
        agt.run();
    }
}