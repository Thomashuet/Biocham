package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;
import javax.swing.JPanel;



/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham boolean simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class ImagePanel extends JPanel {

	  private Image img;

	  public ImagePanel(String img) {
	    this(Utils.createImage(img));
	  }
	  
	  public ImagePanel(Image img) {
	    this.img = img;
	    Dimension size = new Dimension(img.getWidth(null), img.getHeight(null));
	    setPreferredSize(size);
	    setMinimumSize(size);
	    setMaximumSize(size);
	    setSize(size);
	    setLayout(null);
	  }

	  public void paintComponent(Graphics g) {
	    g.drawImage(img, 0, 0, null);
	  }

	}
