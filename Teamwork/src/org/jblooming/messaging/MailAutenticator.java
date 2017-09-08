package org.jblooming.messaging;

import javax.mail.Authenticator;
import javax.mail.PasswordAuthentication;

public class MailAutenticator extends Authenticator {
  private PasswordAuthentication uidPwdRepository;

  public MailAutenticator(String uid, String pwd) {
    uidPwdRepository = new PasswordAuthentication(uid, pwd);
  }

  protected PasswordAuthentication getPasswordAuthentication() {
    return uidPwdRepository;
  }


}
