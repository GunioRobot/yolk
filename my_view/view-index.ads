with AWS.Response;
with AWS.Status;

package View.Index is

   function Generate (Request : in AWS.Status.Data) return AWS.Response.Data;
   --  Generate the content for the Index resource.

end View.Index;
