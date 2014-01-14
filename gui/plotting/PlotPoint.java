package fr.inria.contraintes.biocham.plotting;



import java.io.Serializable;

//////////////////////////////////////////////////////////////////////////
//// PlotPoint

/**
 A simple structure for storing a plot point.
 @author Edward A. Lee
 @version $Id: PlotPoint.java,v 1.37 2005/07/08 19:59:36 cxh Exp $
 @since Ptolemy II 0.2
 @Pt.ProposedRating Yellow (cxh)
 @Pt.AcceptedRating Yellow (cxh)
 */
public class PlotPoint implements Serializable {
    ///////////////////////////////////////////////////////////////////
    ////                         public variables                  ////

    /** True if this point is connected to the previous point by a line. */
    public boolean connected = false;

    /** True if the yLowEB and yHighEB fields are valid. */
    public boolean errorBar = false;

    /** Original value of x before wrapping. */
    public double originalx;

    /** X value after wrapping (if any). */
    public double x;

    /** Y value. */
    public double y;

    /** Error bar Y low value. */
    public double yLowEB;

    /** Error bar Y low value. */
    public double yHighEB;
}