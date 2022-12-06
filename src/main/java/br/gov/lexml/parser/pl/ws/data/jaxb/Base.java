package br.gov.lexml.parser.pl.ws.data.jaxb;

import org.pojomatic.Pojomatic;
import org.pojomatic.annotations.AutoProperty;

/**
 * <p>Abstract Base class.</p>
 *
 * @author joao
 * @version $Id: $Id
 */
@AutoProperty
@jakarta.xml.bind.annotation.XmlTransient
public abstract class Base {
	  /** {@inheritDoc} */
	  @Override final public boolean equals(Object o) {
	    return Pojomatic.equals(this, o);
	  }

	  /** {@inheritDoc} */
	  @Override final public int hashCode() {
	    return Pojomatic.hashCode(this);
	  }

	  /** {@inheritDoc} */
	  @Override final public String toString() {
	    return Pojomatic.toString(this);
	  }
}
