package org.jblooming.ontology;

import org.jblooming.persistence.hibernate.PersistenceContext;

/**
 * Created by rbicchierai on 30/03/2017.
 * Questa interfaccia deve essere implementata da tutti gli oggetti che hanno bisogno di aggiornare dei campi denormalizzati prima del salvataggio finale su db in modo ottimizzato.
 * Ovvero se obj.store()  viene chiamato n. volte l'aggiornamento viene fatto una sola volta.
 *
 * Lo store(pc) di identifiable support controllerà se l'oggetto implementa defferredUpdateNeed  e lo mette in un set ordinato per ordine di inserimento sul persistentcontext.
 * Sia pc.checkpoint che pc.commitAndClose prima di fare la chiusura dovranno chiamare il metodo beforeStore() sugli oggetti della lista e rimuoverli
 *
 *
 */
public interface HasDenormalizedFields {

  /**
   * Deve essere chiamata quando un oggetto dipendente ritiene di aver "sporcato" l'oggetto da cui dipende.
   * Ad esempio quando cancello una issue potrei dover aggiornare dei conteggi sul task -> task.enqueueForUniqueDefferredStore()
   */
  void markAsDirty();

  /**
   * viene chiamata prima del commit
   * In questo metodo deve essere messo il codice che effettua gli aggiornamenti
   *
   * @param pc
   */
  void recomputeDenormalizedFields(PersistenceContext pc);


}
