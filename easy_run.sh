visualize=false

# Check for command-line arguments
while getopts ":v" opt; do
    case ${opt} in
        v)
            visualize=true
            ;;
        \?)
            exit 1
            ;;
    esac
done

cargo run -- -i output/source.j -o output/source.s
cd output
make source
echo "Running program"
./source
echo "Program finished with exit code" $?

cd ../
if [ "$visualize" = true ]; then
	python3 scripts/display_ast.py
	echo "Visualizing abstract syntax tree"
fi

echo "Cleaning up"
rm temp_tree_visualize_data