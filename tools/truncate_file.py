#!/usr/bin/env python3
"""
Generate truncated copies of a file for parser error recovery testing.
By default each truncated copy is 10 characters shorter than the previous one.
"""

import sys
import os
import argparse


def truncate_file(input_path, output_dir, step=10):
    with open(input_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    file_size = len(content)
    basename = os.path.basename(input_path)
    name, ext = os.path.splitext(basename)
    
    os.makedirs(output_dir, exist_ok=True)
    
    count = 0
    for length in range(file_size, 0, -step):
        truncated = content[:length]
        output_name = f"{name}_trunc_{count:04d}_{length:05d}{ext}"
        output_path = os.path.join(output_dir, output_name)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(truncated)
        
        count += 1
    
    print(f"Created {count} truncated copies in {output_dir}")
    print(f"Original size: {file_size} characters")
    print(f"Smallest size: {max(0, file_size % step)} characters")


def main():
    parser = argparse.ArgumentParser(
        description='Generate truncated copies of a file for parser testing',
        epilog='Example: %(prog)s fn/listutils.fn /tmp/truncated'
    )
    parser.add_argument('input_file', help='File to truncate')
    parser.add_argument('output_dir', help='Directory for truncated copies')
    parser.add_argument(
        '--step', 
        type=int, 
        default=10,
        help='Number of characters to remove per truncation (default: 10)'
    )
    
    args = parser.parse_args()
    
    if not os.path.isfile(args.input_file):
        print(f"Error: {args.input_file} is not a file", file=sys.stderr)
        sys.exit(1)
    
    truncate_file(args.input_file, args.output_dir, args.step)


if __name__ == '__main__':
    main()
