package org.jblooming.waf;

import org.jblooming.security.Securable;
import org.jblooming.security.Permission;
import org.jblooming.operator.Operator;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.view.PageState;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: May 25, 2007
 * Time: 10:12:12 AM
 */
public class PagePlugin extends PageQuark {

  
  public Class willBeActiveWhenMainObjectIsOfClass;
  public boolean isForSingleObject = true;

  /**
   * the main object is used when dra
   */
  public Securable mainObject;


   public PagePlugin() {
   }
  
  public PagePlugin(Class willBeActiveWhenMainObjecyIsOfClass) {
    this.willBeActiveWhenMainObjectIsOfClass = willBeActiveWhenMainObjecyIsOfClass;
  }

  /**
   * @param logged if is null return true only when there is no permission required
   * @return true if the user has permission on delegated object.
   */
  public boolean isVisibleFor(Operator logged) {
    boolean ret = false;

    if (mainObject != null) {
      if (permissions.size() > 0) {
        if (logged != null) {
          for (Permission p : permissions) {
            if (mainObject.hasPermissionFor(logged, p)) {
              ret = true;
              break;
            }
          }
        }
      } else {
        ret = true;
      }
    } else {
      ret = super.isVisibleFor(logged);
    }

    return ret;
  }

  /**
   * is sensate to see this part in the current page? The mainObject is of the same kind? the logged user is authorized?
   *
   * @param pageState is used to get the mainObject and the logged user
   * @return
   */

  public boolean isVisibleInThisContext(PageState pageState) {
    boolean ret = false;
    // test if the main object is of the same kind in case of isForSingleObject or the first object of the page
    if (isForSingleObject) {
      if (pageState.getMainObject() != null && ReflectionUtilities.instanceOf(pageState.getMainObject(),willBeActiveWhenMainObjectIsOfClass)) {
        Operator logged = null;
        logged = pageState.getLoggedOperator();
        ret = isVisibleFor(logged);
      }
    } /*else {
      if (pagestate.getPage() != null) {
        Object sample=pagestate.getPage().getSampleElement();
        if (sample!=null && sample.getClass().equals(willBeActiveWhenMainObjectIsOfClass)){
          ret=true;
        }

      }
    }*/
    return ret;
  }


}
