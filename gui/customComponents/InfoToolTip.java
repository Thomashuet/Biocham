package fr.inria.contraintes.biocham.customComponents;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;

import javax.swing.JComponent;
import javax.swing.JToolTip;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ToolTipUI;
import javax.swing.plaf.basic.BasicToolTipUI;


/**
 * Class that creates a custom JToolTip component that holds an image.
 * 
 * @author Dragana Jovanovska  
 */ 
public class InfoToolTip extends JToolTip{

	  String tipText;
	
	  public InfoToolTip(String text) {	
		 //setTipText(text);
	     setUI(new InfoToolTipUI(text));
	   
	    
	  }

	
		
}


class InfoToolTipUI extends ToolTipUI {

	Color color1=new Color(153,204,255);
	Color color2=new Color(230,230,230);
	Color color3=new Color(204,204,204);
	
	 /** The default Border around the JToolTip. */
	InfoTipStyle modern;
	//String text;
	 

		/** The shared instance of BasicToolTipUI used for all ToolTips. */
		private static BasicToolTipUI shared;

	  /**
	   * Creates a new BasicToolTipUI object.
	   */
	  public InfoToolTipUI(String filepath)
	  {
	    super();
	    modern=new InfoTipStyle(filepath,10,10,color1,color2,color3);
	    modern.setBorderThickness(3);
		modern.enableAntiAliasing(true);
		//text=t;
	  }

	  /**
	   * This method creates a new BasicToolTip UI for the given 
		 * JComponent.
	   *
	   * @param c The JComponent to create a UI for.
	   *
	   * @return A BasicToolTipUI that can be used by the given JComponent.
	   */
	  public static ComponentUI createUI(JComponent c)
	  {
			if (shared == null)
				shared = new BasicToolTipUI();
			return shared;
	  }

	  /**
	   * This method returns the msximum size of the given JComponent.
	   *
	   * @param c The JComponent to find a maximum size for.
	   *
	   * @return The maximum size.
	   */
	  public Dimension getMaximumSize(JComponent c)
	  {
	    return getPreferredSize(c);
	  }

	  /**
	   * This method returns the minimum size of the given JComponent.
	   *
	   * @param c The JComponent to find a minimum size for.
	   *
	   * @return The minimum size.
	   */
	  public Dimension getMinimumSize(JComponent c)
	  {
	    return getPreferredSize(c);
	  }

	  /**
	   * This method returns the preferred size of the given JComponent.
	   *
	   * @param c The JComponent to find a preferred size for.
	   *
	   * @return The preferred size.
	   */
	  public Dimension getPreferredSize(JComponent c)
	  {
	    JToolTip tip = (JToolTip) c;
	    Rectangle vr = new Rectangle();
	    Rectangle ir = new Rectangle();
	    Rectangle tr = new Rectangle();
	    Insets insets = tip.getInsets();
	    FontMetrics fm = tip.getFontMetrics(tip.getFont());
	    SwingUtilities.layoutCompoundLabel(tip, fm, tip.getToolTipText(), null,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER, vr, ir, tr, 0);
	    
	    return new Dimension(modern.getWidth(),
	                         modern.getHeight());
	  }

	  /**
	   * This method installs the defaults for the given JComponent.
	   *
	   * @param c The JComponent to install defaults for.
	   */
	  protected void installDefaults(JComponent c)
	  {
	    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
	    //c.setBackground(Color.white);//defaults.getColor("ToolTip.background")
	    c.setForeground(Color.BLUE.darker());//defaults.getColor("ToolTip.foreground")
	    c.setFont(defaults.getFont("ToolTip.font"));
	    c.setBorder(modern);
	   
	  }

	  /**
	   * This method installs the listeners for the given JComponent.
	   *
	   * @param c The JComponent to install listeners for.
	   */
	  protected void installListeners(JComponent c)
	  {
	  }

	  /**
	   * This method installs the UI for the given JComponent.
	   *
	   * @param c The JComponent to install the UI for.
	   */
	  public void installUI(JComponent c)
	  {
	    c.setOpaque(true);
	    installDefaults(c);
	    installListeners(c);
	  }

	  /**
	   * This method paints the given JComponent with the given Graphics object.
	   *
	   * @param g The Graphics object to paint with.
	   * @param c The JComponent to paint.
	   */
	  public void paint(Graphics g, JComponent c)
	  {
	    JToolTip tip = (JToolTip) c;

	    if (tip.getToolTipText() == null)
	      return;

	    Rectangle vr = new Rectangle();
	    vr = SwingUtilities.calculateInnerArea(tip, vr);
	    Rectangle ir = new Rectangle();
	    Rectangle tr = new Rectangle();
	    FontMetrics fm = tip.getFontMetrics(tip.getFont());
	    SwingUtilities.layoutCompoundLabel(tip, fm, tip.getToolTipText(), null,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER,
	                                       SwingConstants.CENTER, vr, ir, tr, 0);

	    Color saved = g.getColor();
	    g.setColor(Color.BLACK);
	    c.setBorder(modern);
	    g.drawString("", 1, 1);
	    g.setColor(saved);
	  }

	  /**
	   * This method uninstalls the defaults for the given JComponent.
	   *
	   * @param c The JComponent to uninstall defaults for.
	   */
	  protected void uninstallDefaults(JComponent c)
	  {
	    c.setForeground(null);
	    c.setBackground(null);
	    c.setFont(null);
	    c.setBorder(null);
	  }

	  /**
	   * This method uninstalls listeners for the given JComponent.
	   *
	   * @param c The JComponent to uninstall listeners for.
	   */
	  protected void uninstallListeners(JComponent c)
	  {
	  }

	  /**
	   * This method uninstalls the UI for the given JComponent.
	   *
	   * @param c The JComponent to uninstall.
	   */
	  public void uninstallUI(JComponent c)
	  {
	    uninstallDefaults(c);
	    uninstallListeners(c);
	  }
}