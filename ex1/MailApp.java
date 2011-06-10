public interface MailApp {
    public void setMailer (Mailer m) ;
    public String getFromAddr () throws Exception;
    public void run () throws Exception;
}
