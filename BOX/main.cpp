#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
//#include "box.h"
#include "boxbox.h"

void __cdecl input_proc(BOX_CALLBACK_STRUCT *input) {
	input->size = fread(input->buffer, sizeof(char), input->size, (FILE*) input->handle);
}

void __cdecl output_proc(BOX_CALLBACK_STRUCT *output) {
	fwrite(output->buffer, sizeof(char), output->count, (FILE*) output->handle);
}

// User interface.  Args are input and output file.
int main(int argc, char **argv) {

	// Check arguments
	if ((argc != 4) || ((argv[1][0] != 'c') && (argv[1][0] != 'd'))) {
		printf("<exe> c/d <input> <output>\n");
		return 1;
	}

	// Get start time
	clock_t start = clock();

	// Compress
	FILE *in = fopen(argv[2], "rb");
	if (!in) perror(argv[2]), exit(1);
	FILE *out = fopen(argv[3], "wb");
	if (!out) perror(argv[3]), exit(1);

	unsigned char *inbuf = (unsigned char *) malloc(1 << 20);
	unsigned char *outbuf = (unsigned char *) malloc(1 << 20);
	void *mem = malloc(box_workmem_size());

	BOX_CALLBACK_STRUCT input;
	input.callback = &input_proc;
	input.handle = (void*) in;
	input.buffer = inbuf;
	input.size = 1 << 20;

	BOX_CALLBACK_STRUCT output;
	output.callback = &output_proc;
	output.handle = (void*) out;
	output.buffer = outbuf;
	output.size = 1 << 20;

	if (argv[1][0] == 'c') {
		box_compress(&input, &output, mem);
	} else {
		box_decompress(&input, &output, mem);
	}

	free(inbuf);
	free(outbuf);
	free(mem);

	// Report result
	printf("%ld -> %ld in %1.2f sec.\n",
		ftell(in), ftell(out), double(clock() - start) / CLOCKS_PER_SEC);

	fclose(in);
	fclose(out);
	return 0;
}

