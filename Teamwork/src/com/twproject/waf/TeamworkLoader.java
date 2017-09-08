package com.twproject.waf;

import com.opnlb.fulltext.IndexingMachine;
import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.waf.settings.TeamworkSettings;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.AccessControlFilter;
import org.jblooming.waf.FrontControllerFilter;
import org.jblooming.waf.configuration.LoaderSupport;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;

import javax.servlet.ServletContextEvent;
import java.util.List;

public class TeamworkLoader extends LoaderSupport {

  public void configApplications() {

    // keeep it as last otherwise the defaultOperatorSubclass is lost!
    TeamworkSettings teamworkSettings = new TeamworkSettings();
    ApplicationState.platformConfiguration.addApplication(teamworkSettings);

    AccessControlFilter.LOGIN_PAGE_PATH_FROM_ROOT = "/applications/teamwork/security/login.jsp";
    FrontControllerFilter.ERROR_PAGE_PATH_FROM_ROOT = "/applications/teamwork/administration/error.jsp";
    ApplicationState.platformConfiguration.defaultIndex = "/applications/teamwork/index.jsp";
    ApplicationState.platformConfiguration.defaultApplication = teamworkSettings;
  }

  public void createDefaultOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {
    //postpone it
  }

  public void createTeamworkDefaultOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {

    OqlQuery q;
    if (pc == null)
      q = new OqlQuery("from " + Operator.class.getName() + " as op where op.administrator = :admin ");
    else
      q = new OqlQuery("from " + Operator.class.getName() + " as op where op.administrator = :admin ", pc);

    //OqlQuery q = new OqlQuery("from " + Operator.class.getName() + " as op where op.administrator = :admin ", pc);
    q.getQuery().setBoolean("admin", true);
    q.getQuery().setMaxResults(1);

    List opL = q.list();

    if (opL == null || opL.size() == 0) {
      Operator administrator = null;
      try {
        administrator = (Operator) PlatformConfiguration.defaultOperatorSubclass.newInstance();
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }

      administrator.setAdministrator(true);
      administrator.setEnabled(true);
      administrator.setLoginName("administrator");
      administrator.changePassword( "");
      administrator.setName("System");
      administrator.setSurname("Manager");
      administrator.putOption(WebSiteConstants.HOME_PAGE, "createCompany.jsp");
      administrator.store();

      Person person = new Person();
      person.setPersonName("System");
      person.setPersonSurname("Manager");
      person.setHidden(false);
      person.setMyself((TeamworkOperator) administrator);
      person.store();

      AnagraphicalData ad = new AnagraphicalData();
      ad.setIdAsNew();
      ad.store();
      person.getAnagraphicalDatas().add(ad);


      Tracer.platformLogger.info("open lab platform - created default operator administrator");
    }
  }

  public void createSystemOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {
    //postpone it
  }

  public void createTeamworkSystemOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {
    OqlQuery q;
    if (pc == null)
      q = new OqlQuery("from " + Operator.class.getName() + " as op where op.loginName = :system ");
    else
      q = new OqlQuery("from " + Operator.class.getName() + " as op where op.loginName = :system ", pc);
    q.getQuery().setString("system", "system");
    q.getQuery().setMaxResults(1);

    List opL = q.list();

    if (opL == null || opL.size() == 0) {
      Operator system = null;
      try {
        system = (Operator) PlatformConfiguration.defaultOperatorSubclass.newInstance();
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
      system.setAdministrator(true);
      system.setLoginName("system");

      /*STANISLAO MOULINSKY
Acerrimo nemico di Carter, finto barone spagnolo di origine russa,
è l'asso dei travestimenti: quando si traveste il suo corpo cambia totalmente.
Si è travestito praticamente da tutto: Contessa spagnola, Uomo delle fogne, armadio,
Ricetrasmittente da guerra, Angelo viola, maggiordomo, cadavere di banchiere...
Frase celebre:
"Ebbene sì, maledetto Carter, hai vinto anche stavolta!"*/
      system.changePassword("stanislaomoulinsky");
      system.setName("System");
      system.setSurname("Operator");
      system.setEnabled(false);

      system.store();

      Person person = new Person();
      person.setPersonName("Teamwork");
      person.setPersonSurname("Machine");
      person.setHidden(true);
      person.setMyself((TeamworkOperator) system);
      person.store();

      Tracer.platformLogger.info("open lab platform - created default system operator");
    }
  }

   public void contextDestroyed(ServletContextEvent event) {

    // stop indexer
    IndexingMachine.stop();

    // stop jbpm scheduler

    // call super
    super.contextDestroyed(event);

  }


}
