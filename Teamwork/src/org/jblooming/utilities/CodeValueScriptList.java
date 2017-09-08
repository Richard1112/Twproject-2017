package org.jblooming.utilities;

import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: angantcha
 * Date: 28-set-2006
 * Time: 15.45.50
 * To change this template use File | Settings | File Templates.
 */
public class CodeValueScriptList extends CodeValueList{
  private Map<String ,String> codeValuesScript = new HashTable <String,String>();


  public void add(String code, String value,String script) {
    getCodeValuesScript().put(code,script);
    this.add(code,value);
  }

  public Map<String, String> getCodeValuesScript() {
    return codeValuesScript;
  }

}
