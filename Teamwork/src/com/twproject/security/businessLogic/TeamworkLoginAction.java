package com.twproject.security.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import org.jblooming.security.License;
import com.twproject.security.TeamworkArea;
import com.twproject.waf.TeamworkLoader;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.LdapUser;
import org.jblooming.security.Role;
import org.jblooming.security.businessLogic.LoginAction;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.naming.NamingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Currency;
import java.util.Locale;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 29, 2008
 * Time: 3:45:58 PM
 */
public class TeamworkLoginAction extends LoginAction {


  public void login(PageState pageState, HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ApplicationException {


    super.login(pageState, request, response);

    //check if license is about to expire
    try {
      long daysRemaining = (License.fromFile().expires.getTime() - System.currentTimeMillis()) / CompanyCalendar.MILLIS_IN_DAY;
      if (daysRemaining < 7) {
        ApplicationState.applicationParameters.put("LIC_EXPIRES", daysRemaining+"");
      } else
        ApplicationState.applicationParameters.remove("LIC_EXPIRES");
    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
    }

    // store an hit to gain points
    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
    if (loggedOperator != null) {

      Hit.getInstanceAndStore(loggedOperator, loggedOperator, 1);

      boolean justSetupped = Fields.TRUE.equals(ApplicationState.applicationSettings.get("JUST_SETUPPED"));
      //short lived
      if (justSetupped) {
        ApplicationState.applicationSettings.remove("JUST_SETUPPED");
        ApplicationState.dumpApplicationSettings();

        //store the global language with hacks for easy start
        if (loggedOperator.hasPermissionAsAdmin()) {
          if (JSP.ex(pageState.getEntry(OperatorConstants.FLD_SELECT_LANG))) {
            String localeString = pageState.getEntry(OperatorConstants.FLD_SELECT_LANG).stringValueNullIfEmpty();
            ApplicationState.applicationSettings.put(OperatorConstants.FLD_SELECT_LANG, localeString);

            //also change current session!
            pageState.sessionState.resetLocaleAndTimeZone();

            //get symbol
            Locale locale = I18n.getLocale(localeString);
            String currencySymbol = "";
            try {
              currencySymbol = Currency.getInstance(locale).getSymbol();
            } catch (Throwable e) {
            }

            currencySymbol = StringUtilities.replaceAllNoRegex(currencySymbol, ".", "");
            currencySymbol = StringUtilities.replaceAllNoRegex(currencySymbol, ",", "");
            currencySymbol = StringUtilities.replaceAllNoRegex(currencySymbol, "#", "");
            currencySymbol = StringUtilities.replaceAllNoRegex(currencySymbol, "0", "");

            if ("EUR".equalsIgnoreCase(currencySymbol))
              currencySymbol = "€";
            else if ("GBP".equalsIgnoreCase(currencySymbol))
              currencySymbol = "£";

            //DecimalFormatSymbols dfs = new DecimalFormatSymbols(locale);
            //String baseCurrencyFormat = "###"+dfs.getGroupingSeparator()+"##0"+dfs.getDecimalSeparator()+"00";
            String baseCurrencyFormat = "###,##0.00";


            ApplicationState.applicationSettings.put(SystemConstants.CURRENCY_FORMAT, baseCurrencyFormat + currencySymbol);

            TeamworkLoader.applySystemSettings();
            ApplicationState.dumpApplicationSettings();
          }
        }
      }
    }
  }


  protected Operator createPlatformUserFromLDAP(String username, PageState pageState) {

    //find a default area
    String areaId = ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_IN_AREA);

    if (!JSP.ex(areaId))


      throw new PlatformRuntimeException("To import users from LDAP at login you must set an area");

    try {
      TeamworkArea a = (TeamworkArea) PersistenceHome.findByPrimaryKey(TeamworkArea.class, areaId);
      LdapUser lu = LdapUtilities.getLdapUser(username, LdapUtilities.getDefaultContext());

      Person person = LdapAction.createTeamworkPerson(true, null, lu, null, a);
      Role op = a.getOperationalRole();
      if (op != null)
        person.getMyself().addRoleAndPersist(op);

      PersistenceContext.getDefaultPersistenceContext().checkPoint();
      Tracer.platformLogger.info("Created a Twproject user from an LDAP account: " + username);
      //otherwise the initialization in http session fails afterwards
      PersistenceContext.getDefaultPersistenceContext().session.evict(person);
      PersistenceContext.getDefaultPersistenceContext().session.evict(person.getMyself());

      return person.getMyself();

    } catch (PersistenceException e) {
      throw new PlatformRuntimeException(e);
    } catch (NamingException e) {
      throw new PlatformRuntimeException(e);
    }
  }


}
