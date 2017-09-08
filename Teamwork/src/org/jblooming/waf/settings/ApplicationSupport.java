package org.jblooming.waf.settings;

import org.jblooming.security.Permission;
import org.jblooming.security.Permissions;
import org.jblooming.waf.ScreenBasic;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;
import java.util.HashSet;
import java.util.Set;

public abstract class ApplicationSupport implements Application {

  String version = null;


    protected ApplicationSupport(Permissions permissionsImpl) {
        this(PlatformConfiguration.defaultOperatorSubclass, permissionsImpl);
    }

    protected ApplicationSupport(Class defaultOperatoSubClass, Permissions permissionsImpl) {
        PlatformConfiguration.defaultOperatorSubclass = defaultOperatoSubClass;
        if (permissionsImpl != null)
            Permission.addPermissions(permissionsImpl, permissions);
    }

    protected ApplicationSupport(Class defaultOperatoSubClass, Permissions[] permissionsImpl) {
        PlatformConfiguration.defaultOperatorSubclass = defaultOperatoSubClass;
        if (permissionsImpl != null) {
            for (int i = 0; i < permissionsImpl.length; i++) {
                Permissions permissions1 = permissionsImpl[i];
                Permission.addPermissions(permissions1, permissions);
            }
        }
    }

    protected Set<Permission> permissions = new HashSet<Permission>();

    public Set<Permission> getPermissions() {
        return permissions;
    }


    public Class getDefaultScreenClass() {
        return getDefaultScreenInstance().getClass();
    }

    public ScreenBasic getDefaultScreenInstance() {
        return null;
    }

    public void configureNeedingPageContext(PageContext pageContext) {
    }

    public void configBeforePerform(HttpServletRequest request) {
    }

    // add graziella - 23/10/2008

    public void sessionStateValueBound() {

    }

    public void sessionStateValueUnbound() {

    }

  public void applicationDestroy() {
    // in specific applications styop scheduler etc.
  }

}
