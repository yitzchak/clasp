/*
    File: claspMpi.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#ifdef USE_OPENCL
#include <OpenCL/opencl.h>
#endif
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/cons.h>
#include <clasp/core/lispStream.h>
#include <clasp/openclp/claspOpenCL.h>
#include <clasp/core/wrappers.h>


#ifdef USE_OPENCL
namespace openclp {
#define DATA_SIZE (1024)

const char *KernelSource =
  "__kernel void square(__global float* input, __global float* output, const unsigned int count) { \n" \
  "   int i = get_global_id(0);                                                                    \n" \
  "   if(i < count) { output[i] = input[i] * input[i]; }                                           \n" \
  "}";


CL_DEFUN void opencl__run_demo() {
  int err;
  cl_device_id device_id;
  clGetDeviceIDs(NULL, CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);
  cl_context context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
  cl_command_queue commands = clCreateCommandQueue(context, device_id, 0, &err);
  cl_program program = clCreateProgramWithSource(context, 1, (const char **) & KernelSource, NULL, &err);
  clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
  cl_kernel kernel = clCreateKernel(program, "square", &err);
  cl_mem input = clCreateBuffer(context,  CL_MEM_READ_ONLY,  sizeof(float) * DATA_SIZE, NULL, NULL);
  cl_mem output = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(float) * DATA_SIZE, NULL, NULL);
  float data[DATA_SIZE];
  for (int i = 0; i < DATA_SIZE; i++) { data[i] = i; }
  err = clEnqueueWriteBuffer(commands, input, CL_TRUE, 0, sizeof(float) * DATA_SIZE, data, 0, NULL, NULL);
  clSetKernelArg(kernel, 0, sizeof(cl_mem), &input);
  clSetKernelArg(kernel, 1, sizeof(cl_mem), &output);
  unsigned int count = DATA_SIZE;
  clSetKernelArg(kernel, 2, sizeof(unsigned int), &count);
  size_t local;
  clGetKernelWorkGroupInfo(kernel, device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, NULL);
  size_t global = count;
  clEnqueueNDRangeKernel(commands, kernel, 1, NULL, &global, &local, 0, NULL, NULL);
  clFinish(commands);
  float results[DATA_SIZE];
  clEnqueueReadBuffer(commands, output, CL_TRUE, 0, sizeof(float) * count, results, 0, NULL, NULL);
  unsigned int correct = 0;
  for (int i = 0; i < count; i++) {
    if (results[i] == data[i] * data[i]) { correct++; }
  }
  printf("Computed '%d/%d' correct values!\n", correct, count);
  clReleaseMemObject(input);
  clReleaseMemObject(output);
  clReleaseProgram(program);
  clReleaseKernel(kernel);
  clReleaseCommandQueue(commands);
  clReleaseContext(context);
  return;
}


};
#endif
