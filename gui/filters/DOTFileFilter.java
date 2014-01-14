
package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * A class that keeps files different from DOT files (with .dot extension)
 * from appearing in the directory listing.
 *            
 * @author Dragana JOVANOVSKA         
*/
public class DOTFileFilter extends FileFilter {

   public boolean accept(File f) {
      return (f.isDirectory() || f.getName().endsWith(".dot"));
   }

   public String getDescription() {
      return "DOT files only";
   }
}
