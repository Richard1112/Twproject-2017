/*
 * Created by Roberto Bicchierai and Pietro Polsinelli.
 * User: Pietro Polsinelli
 * Date: Jul 4, 2002
 * Time: 12:11:14 PM
 */
package org.jblooming.waf.view;

import org.jblooming.ontology.Identifiable;
import org.jblooming.utilities.HashTable;

import java.util.*;


public class ClientEntries {

  private Map<String,ClientEntry> clientEntries = new LinkedHashMap<String,ClientEntry>();


  public ClientEntries addEntry(ClientEntry ce) {
    clientEntries.put(ce.name, ce);
    return this;
  }

  public ClientEntries addEntry(String name, String value) {
    ClientEntry ce = new ClientEntry(name, value);
    clientEntries.put(ce.name, ce);
    return this;
  }

  public ClientEntry getEntry(String key) {
    return (ClientEntry) clientEntries.get(key);
  }

  public Set<String> getEntryKeys() {
    return clientEntries.keySet();
  }

  public void addEntries(ClientEntries ce) {
    if (ce != null && ce.clientEntries != null) clientEntries.putAll(ce.clientEntries);
  }

  public void addEntries(Collection<ClientEntry> ces) {
   for (ClientEntry ce : ces) {
     clientEntries.put(ce.name,ce);
   }
  }

  public int size() {
    return clientEntries.size();
  }

  public void deleteEntry(String key) {
    clientEntries.remove(key);
  }

  public void addRequiredEntry(String name, String value) {
    ClientEntry ce = new ClientEntry(name, value);
    ce.required = true;
    clientEntries.put(ce.name, ce);
  }

  public String toString(){
    StringBuffer ret= new StringBuffer();
    for (ClientEntry ce : clientEntries.values()){
      ret.append(ce.name).append(":").append(ce.stringValueNullIfEmpty()).append("\n");
    }
    return ret.toString();
  }

  public Collection<ClientEntry> getClientEntries() {
    return clientEntries.values();
  }

  public Map<String,ClientEntry> getEntriesStartingWithStripped(String prefix) {
    Map<String,ClientEntry> selected = new HashTable<String,ClientEntry>();
    for (ClientEntry ce : getClientEntries()) {
      if (ce.name.startsWith(prefix))
        selected.put(ce.name.substring(prefix.length()),ce);
    }
    return selected;
  }

  /**
   *
   * @param prefix
   * @param requestedValue
   * @return can be used to get the ids of a selection of checked CheckField ("prefix_"+objId) as follow
   *  Set<String> ids= pageState.getClientEntries().getEntriesStartingWithStripped("prefix_", Fields.TRUE).keySet();
   */
   public Map<String,ClientEntry> getEntriesStartingWithStripped(String prefix, String requestedValue) {
     Map<String,ClientEntry> selected = new HashTable<String,ClientEntry>();
      for (ClientEntry ce : getClientEntries()) {
        if (ce.name.startsWith(prefix) && requestedValue.equals(ce.stringValueNullIfEmpty()))
          selected.put(ce.name.substring(prefix.length()),ce);
      }
    return selected;

   }

  public boolean validEntries() {
    boolean result  = false;
    for (ClientEntry ce : clientEntries.values()) {
      if (ce.errorCode!=null) {
        result = true;
        break;
      }
    }
    return !result;
  }


  public String getErrorString() {
    String result  = "";
    for (ClientEntry ce : clientEntries.values()) {
      if (ce.errorCode!=null) {
        result = result + ce.name+ " [value:\""+ce.stringValueNullIfEmpty()+"\"] error:"+ce.errorCode+"\n";
        break;
      }
    }
    return result;
  }

  public boolean hasEntry(String name){
    return clientEntries!=null && clientEntries.containsKey(name);
  }


}
