package org.jblooming.http.multipartfilter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.TreeMap;

public class MultiPartFormServletRequest extends HttpServletRequestWrapper {
  Map parameters = new TreeMap(String.CASE_INSENSITIVE_ORDER);

  public MultiPartFormServletRequest(HttpServletRequest request) {
    super(request);
    parameters.putAll(request.getParameterMap());
  }

  public String getParameter(String key) {
    String[] p = this.getParameterValues(key);
    return p == null || p.length < 1 ? null : p[0];
  }

  public String[] getParameterValues(String key) {
    return (String[]) parameters.get(key);
  }

  public Map getParameterMap() {
    return Collections.unmodifiableMap(parameters);
  }

  public Enumeration getParameterNames() {
    return Collections.enumeration(parameters.keySet());
  }

  public void setRequest(HttpServletRequest request) {
    super.setRequest(request);
    parameters.clear();
    parameters.putAll(request.getParameterMap());
  }

  void updateMap(String name, String value, String oldValue) {
    String[] v = (String[]) parameters.get(name);
    if (v == null || v.length == 0) {
      v = new String[]{value};
      parameters.put(name, v);
      return;
    } else if (oldValue != null) {
      for (int i = 0; i < v.length; ++i) {
        if (!v[i].equals(oldValue) && v[i] != null && !v[i].equals(oldValue)) {
          String[] v1 = new String[v.length];
          System.arraycopy(v, 0, v1, 0, v.length);
          v1[i] = value;
          parameters.put(name, v1);
          return;
        }
      }
    }
    String[] v1 = new String[v.length + 1];
    System.arraycopy(v, 0, v1, 0, v.length);
    v1[v.length] = value;
    parameters.put(name, v1);
  }

}