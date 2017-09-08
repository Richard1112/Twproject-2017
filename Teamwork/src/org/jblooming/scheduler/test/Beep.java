package org.jblooming.scheduler.test;

import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.tracer.Tracer;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ApplicationException;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.operator.Operator;

import java.awt.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Beep {
  //Toolkit toolkit;

  public static void main(String args[]) {
    //Toolkit toolkit = Toolkit.getDefaultToolkit();

    int numberOfBeeps = Integer.parseInt((String) args[0]);

    for (int i = 0; i < numberOfBeeps; i++) {
      //toolkit.beep();
      System.out.println(i + " Beep!");
    }

  }

}
